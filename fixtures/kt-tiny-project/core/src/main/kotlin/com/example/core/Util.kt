package com.example.core

import com.example.core.Repo

// Uses Repo, which needs no import: same package, other file.
fun repo(): Repo = Repo()
