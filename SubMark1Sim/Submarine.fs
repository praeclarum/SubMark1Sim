namespace SubMark1Sim

open System
open AppKit
open SceneKit


[<Measure>]
type m
[<Measure>]
type kg
[<Measure>]
type s
[<Measure>]
type rad
[<Measure>]
type deg

[<Measure>]
type N = kg*m/s/s


module SceneUtils =
    let nfloatFromLength (length : float<m>) = nfloat (float length)
    let nfloatFromAngle (length : float<rad>) = nfloat (float length)

    let pi = Math.PI * 1.0<rad>

    let pos (x : float<m>) (y : float<m>) (z : float<m>) =
        SCNVector3(nfloat (float x), nfloat (float y), nfloat (float z))

    let rgb (r : float) (g : float) (b : float) =
        NSColor.FromRgb(nfloat r, nfloat g, nfloat b)

    let radToDeg (angle : float<rad>) = angle * 180.0<deg> / pi

open SceneUtils


type Thruster (radius : float<m>, length : float<m>, xAngle : float<rad>, zAngle : float<rad>) =
    let node = SCNNode.Create ()
    let flameNode = SCNNode.Create ()

    let mutable magnitude = 0.0<N>

    do
        node.Name <- "Thruster"
        node.Geometry <- SCNCylinder.Create(radius=nfloatFromLength radius, height=nfloatFromLength length)
        node.Geometry.FirstMaterial.Diffuse.ContentColor <- rgb 1.0 1.0 1.0
        node.Transform <-
            SCNMatrix4.CreateRotationX(nfloatFromAngle xAngle)
            * SCNMatrix4.CreateRotationZ(nfloatFromAngle zAngle)
        flameNode.Geometry <- SCNCylinder.Create(radius=nfloatFromLength (radius/2.0), height=nfloatFromLength length)
        flameNode.Geometry.FirstMaterial.Diffuse.ContentColor <- rgb 0.3 0.3 1.0
        node.AddChildNode flameNode

    member this.Node = node

    member this.SetMagnitude (newMagnitude : float<N>) =
        magnitude <- newMagnitude
        let flameY = length * magnitude * -1.5<1/N>
        flameNode.Position <- pos 0.0<m> flameY 0.0<m>
        ()


type Submarine () =
    let node = SCNNode.Create()
    let hull = SCNNode.Create()

    let radius = 0.30<m> / 2.0
    let length = 1.0<m>
    let mass = 4.5359237<kg>
    let thrusterRadius = 0.05<m> / 2.0
    let thrusterPitch = pi / 4.0
    let thrusterSep = length * 0.5

    let thrusterBow1 = Thruster(thrusterRadius, radius * 2.0 + 0.2<m>, pi / 4.0, -thrusterPitch)
    let thrusterBow2 = Thruster(thrusterRadius, radius * 2.0 + 0.2<m>, -pi / 4.0, -thrusterPitch)
    let thrusterStern1 = Thruster(thrusterRadius, radius * 2.0 + 0.2<m>, pi / 4.0, thrusterPitch)
    let thrusterStern2 = Thruster(thrusterRadius, radius * 2.0 + 0.2<m>, -pi / 4.0, thrusterPitch)

    let thrusters = [| thrusterBow1; thrusterBow2; thrusterStern1; thrusterStern2 |]

    do
        thrusterBow2.Node.Geometry.FirstMaterial.Diffuse.ContentColor <- rgb 0.5 0.5 0.5
        thrusterStern2.Node.Geometry.FirstMaterial.Diffuse.ContentColor <- rgb 0.5 0.5 0.5
        thrusterBow1.Node.Position <- pos (-thrusterSep/2.0 + 1.5*thrusterRadius) 0.0<m> 0.0<m>
        thrusterBow2.Node.Position <- pos (-thrusterSep/2.0 - 1.5*thrusterRadius) 0.0<m> 0.0<m>
        thrusterStern1.Node.Position <- pos (thrusterSep/2.0 - 1.5*thrusterRadius) 0.0<m> 0.0<m>
        thrusterStern2.Node.Position <- pos (thrusterSep/2.0 + 1.5*thrusterRadius) 0.0<m> 0.0<m>

    do
        node.Name <- "Submarine"
        hull.Name <- "Hull"
        hull.Geometry <- SCNCylinder.Create(radius=nfloatFromLength radius, height=nfloatFromLength length)
        hull.Geometry.FirstMaterial.Diffuse.ContentColor <- rgb 0.9 0.9 0.0
        hull.Opacity <- nfloat 0.1
        //let mutable minBB = SCNVector3.Zero
        //let mutable maxBB = SCNVector3.Zero
        //let _ = hull.GetBoundingBox(&minBB, &maxBB)
        hull.Transform <-
            SCNMatrix4.CreateRotationZ(NMath.PI / nfloat 2.0)
        let nose = SCNNode.FromGeometry(SCNSphere.Create(nfloatFromLength radius))
        nose.Geometry.FirstMaterial.Diffuse.ContentColor <- rgb 0.55 0.55 0.0
        nose.Position <- pos (-length/2.0) 0.0<m> 0.0<m>
        node.AddChildNode nose
        node.AddChildNode hull
        let body = SCNPhysicsBody.CreateDynamicBody()
        let shape = SCNPhysicsShape.Create(node, new SCNPhysicsShapeOptions (ShapeType=SCNPhysicsShapeType.ConvexHull, KeepAsCompound=false))
        body.AffectedByGravity <- false
        body.PhysicsShape <- shape
        body.Mass <- nfloat (float32 mass)
        node.PhysicsBody <- body

        node.AddChildNode thrusterBow1.Node
        node.AddChildNode thrusterBow2.Node
        node.AddChildNode thrusterStern1.Node
        node.AddChildNode thrusterStern2.Node

        ()

    member this.Node = node

    member this.Thrusters = thrusters

    member this.CalculateThrusterVectors () =
        thrusters
        |> Array.map (fun t ->
            let vector = t.Node.ConvertVectorToNode (SCNVector3.UnitY, node)
            let position = t.Node.Position
            printfn "V %O" vector
            position, vector)


type SubmarineController (submarine : Submarine) =
    let thrustPositionAndDirections = submarine.CalculateThrusterVectors ()

    let subNode = submarine.Node
    let subBody = subNode.PhysicsBody

    member this.Update (time : float<s>) =

        let forwardVector = -submarine.Node.PresentationNode.WorldRight
        let upVector = SCNVector3.UnitY
        let pitch = -90.0<deg> + radToDeg (acos (float (SCNVector3.Dot (upVector, forwardVector))) * 1.0<rad>)
        //printfn "forward %A" forwardVector
        //printfn "pitch %A" pitch

        let descendMagnitudes =
            [|
                -0.1<N>
                -0.1<N>
                -0.1<N>
                -0.1<N>
            |]
        let ascendMagnitudes =
            [|
                0.1<N>
                0.1<N>
                0.1<N>
                0.1<N>
            |]
        let strafePortMagnitudes =
            [|
                0.1<N>
                -0.1<N>
                0.1<N>
                -0.1<N>
            |]
        let yawMagnitudes =
            [|
                0.1<N>
                -0.1<N>
                -0.1<N>
                0.1<N>
            |]

        let error = 0.0<deg> - pitch
        let control = error * 0.1<N/deg>

        let forwardMagnitudes =
            [|
                -0.05<N>
                -0.05<N>
                control
                control
            |]

        let magnitudes = ascendMagnitudes
        let magnitudes = forwardMagnitudes

        for i in 0..(thrustPositionAndDirections.Length - 1) do
            let position, direction = thrustPositionAndDirections.[i]
            subBody.ApplyForce (nfloat (float magnitudes.[i]) * direction, position, impulse=false)
            submarine.Thrusters.[i].SetMagnitude magnitudes.[i]
        ()


