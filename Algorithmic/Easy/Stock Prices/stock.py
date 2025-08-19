import pandas as pd
import plotly.graph_objects as go
from urllib.error import URLError
import sys

def plot_stock_data(csv_url: str) -> None:
    """
    Downloads Apple stock data from a given URL and creates a Plotly chart
    to demonstrate different time period alignments for financial data.

    Args:
        csv_url: The URL of the CSV file containing the stock data.
                 The CSV must contain 'Date' and other data columns like
                 'AAPL.Open', 'AAPL.High', 'AAPL.Low', 'AAPL.Volume'.
    """
    try:
        print(f"Fetching data from {csv_url}...")
        df = pd.read_csv(csv_url)
        # Convert 'Date' column to datetime objects for proper plotting
        df['Date'] = pd.to_datetime(df['Date'])
        print("Data fetched successfully.")
    except URLError:
        print(f"Error: Could not retrieve data. Check your internet connection or the URL.", file=sys.stderr)
        return
    except (FileNotFoundError, pd.errors.EmptyDataError):
        print(f"Error: The URL did not point to a valid or non-empty CSV file.", file=sys.stderr)
        return
    except KeyError as e:
        print(f"Error: The CSV file is missing a required column: {e}", file=sys.stderr)
        return

    fig = go.Figure()

    # Define configurations for each trace to be added to the plot
    trace_configs = [
        {'name': 'Open', 'y_col': 'AAPL.Open', 'type': 'scatter', 'alignment': None},
        {'name': 'High (Start-aligned)', 'y_col': 'AAPL.High', 'type': 'scatter', 'alignment': 'start'},
        {'name': 'Low (End-aligned)', 'y_col': 'AAPL.Low', 'type': 'scatter', 'alignment': 'end'},
        {'name': 'Volume (Bar, Middle-aligned)', 'y_col': 'AAPL.Volume', 'type': 'bar', 'alignment': 'middle'},
    ]

    for config in trace_configs:
        trace_props = {
            'name': config['name'],
            'x': df['Date'],
            'y': df[config['y_col']],
        }
        if config['alignment']:
            trace_props['xperiod'] = 'M1'
            trace_props['xperiodalignment'] = config['alignment']

        if config['type'] == 'scatter':
            trace_props['mode'] = 'lines+markers'
            fig.add_trace(go.Scatter(**trace_props))
        elif config['type'] == 'bar':
            # Use a secondary y-axis for volume due to its different scale
            trace_props['yaxis'] = 'y2'
            fig.add_trace(go.Bar(**trace_props))

    # Update the layout for a more informative and professional look
    fig.update_layout(
        title_text="Apple Inc. (AAPL) Stock Data Demonstration",
        xaxis_title="Date",
        yaxis_title="Price (USD)",
        yaxis=dict(showgrid=True),
        yaxis2=dict(
            title="Volume",
            overlaying="y",
            side="right",
            showgrid=False,
        ),
        xaxis=dict(
            showgrid=True,
            ticklabelmode="period"
        ),
        legend_title_text="Data Series"
    )

    print("Displaying plot...")
    fig.show()

def main():
    """
    Main function to run the stock plotting script.
    """
    # The URL for Apple's stock data from a public Plotly dataset
    url = "https://raw.githubusercontent.com/plotly/datasets/master/finance-charts-apple.csv"
    plot_stock_data(url)

if __name__ == "__main__":
    main()
