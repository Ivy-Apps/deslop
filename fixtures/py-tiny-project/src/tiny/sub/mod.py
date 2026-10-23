from ..models import User
from tiny.sub import other


def make() -> User:
    return User(other.NAME)
