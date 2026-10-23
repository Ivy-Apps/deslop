package com.example.model

import com.example.core.repo

data class User(val name: String) {
    fun lookup() = repo().find()
}
