"""Instantiate settings dictionary.

This module reads and builds the settings dictionary.
Settings are defined in a configuration YAML file."""

import os

import yaml
from box import Box


def read_settings():
    """Load config from configuration yaml.

    Returns:
        Box object with all config."""

    config_path = os.path.join("../config/config.yml")

    with open(config_path) as config_file:
        settings_dict = yaml.load(config_file, Loader=yaml.SafeLoader)
    return Box(settings_dict)


settings = read_settings()
