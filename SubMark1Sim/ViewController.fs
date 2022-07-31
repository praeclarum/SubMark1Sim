namespace SubMark1Sim

open System
open AppKit
open Foundation
open SceneKit
open CoreGraphics

open SceneUtils

[<Register("ViewController")>]
type ViewController (handle : System.IntPtr) =
    inherit NSViewController(handle)

    let sceneView = new SCNView(CGRect(0.0, 0.0, 640.0, 480.0))

    let scene = SCNScene.Create()
    let camera = SCNCamera.Create()
    do camera.ZNear <- 0.001
    do camera.ZFar <- 100.0
    let cameraNode = SCNNode.Create()
    do cameraNode.Camera <- camera

    let submarine = Submarine ()
    let controller = SubmarineController submarine

    let positionCamera () =
        let target = submarine.Node.PresentationNode.WorldPosition
        let viewMatrix = SCNMatrix4.LookAt(eye=pos 0.0<m> 0.0<m> 2.0<m>, target=target, up=SCNVector3.UnitY)
        let mutable camMatrix = viewMatrix
        camMatrix.Invert()
        cameraNode.Transform <- camMatrix
        ()

    interface ISCNSceneRendererDelegate

    [<Export("renderer:updateAtTime:")>]
    member this.Update (sceneRenderer : ISCNSceneRenderer, time : float) =
        //printfn "LOOP! %A" time
        controller.Update (time * 1.0<s>)
        positionCamera()
        ()


    override this.ViewDidLoad() =
        base.ViewDidLoad()

        sceneView.Frame <- this.View.Bounds
        sceneView.AutoresizingMask <- NSViewResizingMask.WidthSizable|||NSViewResizingMask.HeightSizable

        sceneView.BackgroundColor <- rgb 0.005 0.1 0.01

        let root = scene.RootNode
        root.AddChildNode cameraNode
        root.AddChildNode submarine.Node

        sceneView.Scene <- scene
        sceneView.PointOfView <- cameraNode
        sceneView.AutoenablesDefaultLighting <- true
        sceneView.AllowsCameraControl <- true
        sceneView.DebugOptions <-
            SCNDebugOptions.ShowPhysicsShapes
            //|||SCNDebugOptions.ShowBoundingBoxes

        let random = Random()
        let gridDistance = 0.75<m>
        let bubbleGeo = SCNSphere.Create(nfloatFromLength 0.01<m>)
        bubbleGeo.FirstMaterial.Diffuse.ContentColor <- rgb 1.0 1.0 1.0
        let grid = SCNNode.Create()
        for xi in -10..10 do
            for yi in -10..10 do
                for zi in -10..10 do
                    let x = (float xi + random.NextDouble() - 0.5) * gridDistance
                    let y = (float yi + random.NextDouble() - 0.5) * gridDistance
                    let z = (float zi + random.NextDouble() - 0.5) * gridDistance
                    let n = SCNNode.Create()
                    n.Position <- pos x y z
                    n.Geometry <- bubbleGeo
                    n.Opacity <- nfloat 0.05
                    grid.AddChildNode n
        root.AddChildNode grid


        positionCamera ()

        this.View.AddSubview(sceneView)

        sceneView.SceneRendererDelegate <- this :> ISCNSceneRendererDelegate
        sceneView.RendersContinuously <- true



    

