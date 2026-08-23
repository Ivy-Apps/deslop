"use client";

import React from 'react';
// Testing even deeper relative imports
import { validateUsername } from '../../../features/login/login-form';
import { capitalize } from '../../../lib/util';

export default function LoginPage() {
  // AI-generated state handler
  const handleLogin = (e: React.FormEvent) => {
    e.preventDefault();
    const isValid = validateUsername("test_user");
    alert(isValid ? "Success" : "Failure");
  };

  return (
    <div>
      <h2>{capitalize("login")}</h2>
      <button onClick={handleLogin}>Log In</button>
    </div>
  );
}