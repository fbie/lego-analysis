//#r "System.Windows.Forms.DataVisualization"

#r "System.Windows.Forms.dll"
#r "System.Windows.Forms.DataVisualization.dll"

open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

let trajectory g k t =
  let evolve (r, f) =
    let dtrf = 0.0001 * r * f
    r + (1.0 - r/k)*r*g - dtrf, dtrf + (1.0 - g)*f
  Seq.scan (fun s _ -> evolve s) (50.0, 10.0) {1..t}

let series = new Series(ChartType=SeriesChartType.Line)
//for x, y in trajectory 0.02 5e2 1500 do
//  series.Points.AddXY(x, y) |> ignore

let area = new ChartArea()
area.AxisX <- new Axis()
area.AxisX.Title <- "Rabbits"
area.AxisX.Minimum <- 0.0
area.AxisY <- new Axis()
area.AxisY.Title <- "Foxes"

let chart = new Chart()
chart.ChartAreas.Add area
chart.Series.Add series

let form = new Form()
form.Controls.Add chart
form.Show()
