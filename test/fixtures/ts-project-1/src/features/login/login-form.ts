import { capitalize } from "../../lib/util";
import {
  mockUser,
  mockCredentials
} from '../../tests/fixtures'

export const validateUsername = (name: string) => {
  // 1. Validate input
  if (name.length < 3) return false;

  // 1a. Log for debug purposes
  console.log(`Validating user: ${capitalize(name)}`);

  // 2. Valid name - return true
  return true;
};