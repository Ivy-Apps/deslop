// AI: Mock data for unit tests
import { formatDate } from "../../src/lib/util";

export const mockUser = {
  id: 1,
  name: "John Doe",
  createdAt: formatDate(new Date()),
};