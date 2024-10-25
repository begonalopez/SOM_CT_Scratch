"""This script runs the data pipeline."""

import pandas as pd

from preprocessing.settings.settings import settings
from preprocessing.data_preprocessing.data_preprocessing_utils import (
    load_data, data_pipeline
)

if __name__ == "__main__":
    print("READING DATA")
    df = load_data(settings.data.path)
    numpy_preprocessed, df_preprocessed = data_pipeline(df)
    df_preprocessed.to_csv(settings.output_data.path)