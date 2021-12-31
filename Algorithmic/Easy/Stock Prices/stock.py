import plotly.graph_objects as go
import pandas as pd

df = pd.read_csv(
    "https://raw.githubusercontent.com/plotly/datasets/master/finance-charts-apple.csv"
)

fig = go.Figure()
fig.add_trace(
    go.Scatter(
        name="Raw Data",
        mode="markers+lines",
        x=df["Date"],
        y=df["AAPL.Open"],
        marker_symbol="star",
    )
)
fig.add_trace(
    go.Scatter(
        name="Start-aligned",
        mode="markers+lines",
        x=df["Date"],
        y=df["AAPL.High"],
        xperiod="M1",
        xperiodalignment="start",
    )
)
fig.add_trace(
    go.Scatter(
        name="Middle-aligned",
        mode="markers+lines",
        x=df["Date"],
        y=df["AAPL.Adjusted"],
        xperiod="M1",
        xperiodalignment="middle",
    )
)
fig.add_trace(
    go.Scatter(
        name="End-aligned",
        mode="markers+lines",
        x=df["Date"],
        y=df["AAPL.Low"],
        xperiod="M1",
        xperiodalignment="end",
    )
)
fig.add_trace(
    go.Bar(
        name="Middle-aligned",
        x=df["Date"],
        y=df["AAPL.Adjusted"],
        xperiod="M1",
        xperiodalignment="middle",
    )
)
fig.update_xaxes(showgrid=True, ticklabelmode="period")
fig.show()
