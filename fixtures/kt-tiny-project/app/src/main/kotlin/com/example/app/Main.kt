package com.example.app

import com.example.core.*
import com.example.model.User
import kotlin.collections.List

fun main() {
    val users: List<User> = listOf(repo().find())
    println(users + AppConfig.name)
}
