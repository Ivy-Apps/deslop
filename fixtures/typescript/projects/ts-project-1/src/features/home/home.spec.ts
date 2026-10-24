import { getHomeData } from "./home-component";
import { mockUser } from "@tests/fixtures/fixtures";
import * from "../../../tests/test-log"

describe("Home Feature", () => {
    it("should return valid home data for the mock user", () => {
        testLog('Home feature test suite')
        const data = getHomeData();
        expect(data.title).toBeDefined();
        expect(mockUser.name).toBe("John Doe");
    });
});