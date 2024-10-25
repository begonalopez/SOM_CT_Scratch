import pandas as pd
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder, MaxAbsScaler, OrdinalEncoder
from sklearn.compose import ColumnTransformer

from preprocessing.settings.settings import settings


def data_pipeline(df):
    """Preprocess the data with a defined pipeline.

    Args:
        df: DataFrame with filtered data.
    Returns:
        DataFrame preprocessed
    """
    # Identify columns
    numeric_columns = settings.data.dtypes.numeric
    ordinal_columns = settings.data.dtypes.ordinal
    categorical_columns = settings.data.dtypes.categorical

    # Transformer creation
    numerical_transformer = Pipeline(steps=[
        ('scaler', MaxAbsScaler())  # Scale values
    ])

    categorical_transformer = Pipeline(steps=[
        ('onehot', OneHotEncoder(handle_unknown='ignore', sparse_output=False))
    ])
    ordinal_transformer = Pipeline(steps=[
        ('ordinal', OrdinalEncoder())
    ])

    # Create preprocessor transformer
    preprocessor = ColumnTransformer(
        transformers=[("num", numerical_transformer, numeric_columns),
                      ("cat", categorical_transformer, categorical_columns),
                      ("ord", ordinal_transformer, ordinal_columns)
        ]
    )

    numpy_preprocessed = preprocessor.fit_transform(df)

    # Getting the column names after transformation
    # Numeric columns remain the same, ordinal columns remain the same,
    # Categorical columns will expand based on one-hot encoding
    categorical_feature_names = preprocessor.named_transformers_['cat'][
        'onehot'].get_feature_names_out(categorical_columns)

    # Combine all feature names
    all_feature_names = numeric_columns + list(
        categorical_feature_names) + ordinal_columns

    # Create a DataFrame with the transformed data
    df_preprocessed = pd.DataFrame(numpy_preprocessed, columns=all_feature_names)

    return numpy_preprocessed, df_preprocessed


def load_data(filepath: str) -> pd.DataFrame:
    """Loads the dataset from the given file path."""
    return pd.read_csv(filepath)
