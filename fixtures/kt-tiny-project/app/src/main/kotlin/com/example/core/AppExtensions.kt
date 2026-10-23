package com.example.core

import com.example.app.AppConfig

// A split package: com.example.core also lives in the :core Gradle module.
fun Repo.label(): String = AppConfig.name
