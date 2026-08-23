import { UserEntity } from "@/domain/user/UserEntity";
import { Result, ok } from "@/shared/Result";

// ALLOWED by domain-purity: imports only from @/domain/** and @/shared/**,
// both of which are listed in the allows clause.
export const findUserById = async (id: string): Promise<Result<UserEntity>> => {
  return ok({ id, email: "user@example.com", name: "User" });
};
