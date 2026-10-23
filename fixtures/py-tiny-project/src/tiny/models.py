from dataclasses import dataclass
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from tiny.core import run  # invisible to the spike: indented


@dataclass
class User:
    name: str
