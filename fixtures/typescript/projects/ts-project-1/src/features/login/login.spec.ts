import { validateUsername } from "./login-form";
import { mockUser } from "@tests/fixtures/fixtures";

describe("Login Feature", () => {
  // AI: Ensure the username from fixtures is valid
  it("should validate the fixture user correctly", () => {
    const result = validateUsername(mockUser.name);
    expect(result).toBe(true);
  });
});