import os
from . import helpers
from tiny.models import User


def run() -> User:
    return helpers.greet(User(os.getlogin()))
