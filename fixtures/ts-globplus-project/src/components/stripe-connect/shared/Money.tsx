// The provider's shared folder: one directory back from any of its service
// folders, which is what {{TARGET_DIR}}/../shared/** reaches.
export function Money({ amount }: { amount: string }) {
  return amount;
}
