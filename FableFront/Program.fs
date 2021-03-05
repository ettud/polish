open FSharp.Control.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open Giraffe
open Giraffe.GiraffeViewEngine
open Giraffe.Serialization.Json

open Thoth.Json.Net
open Thoth.Json.Giraffe

open Models
open Views

let calculateHandler str : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let state = State str
        let newState = state |> Calculator.Calculator().Calculate
        ((match newState.result with 
            | CalculationResult.Error e -> e
            | CalculationResult.Result r -> r.ToString()) |> text) next ctx
                    
let generateHtml view (state : State) (serializer : IJsonSerializer) =
    let curView = view state ignore
    let clientHtml = curView |> Fable.ReactServer.renderToString

    let stateJson = state.GetRecord |> serializer.SerializeToString |> Encode.string |> Encode.toString 0

    html []
      [ head []
          [ 
          ]
        body []
          [ div [_id "elmish-app"] [ rawText clientHtml ]
            script []
              [ rawText (sprintf """
              var __INIT_STATE__ = %s
              """ stateJson) ]
          ]
      ]      

let htmlHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let view = Views.view
        let state = new State()
        ((generateHtml view state (ctx.GetService<IJsonSerializer>())) |> Giraffe.ResponseWriters.htmlView) next ctx

let webApp() : HttpHandler =
  choose [
    routef "/api/calculate/%s" calculateHandler
    route "/test" >=> fun next ctx -> task {
        return! Successful.ok (text "Hello World") next ctx
    }
    route "/" >=> htmlHandler
  ]

let configureApp  (app : IApplicationBuilder) =
  () |> webApp |> app.UseGiraffe

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore
    services.AddSingleton<IJsonSerializer, ThothSerializer>(fun _ -> new ThothSerializer()) |> ignore


[<EntryPoint>]
let main _ =
  WebHost
    .CreateDefaultBuilder()
    .Configure(System.Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls("http://0.0.0.0:5000")
    .Build()
    .Run()
  0
