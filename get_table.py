import pandas as pd
import sys

try:
    assert len(sys.argv) == 3
except AssertionError:
    print("Give a url and name argument.")

try:
    all_tables = pd.read_html(
            sys.argv[1]
    )
    assert all_tables is not None or all_tables != []
    all_tables[0].to_csv('./csv/{}.csv'.format(sys.argv[2]))
except Exception as e:
    print("Something went wrong getting the table from the url")

