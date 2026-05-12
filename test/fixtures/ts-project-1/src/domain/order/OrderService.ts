import { httpGet } from "@/infrastructure/HttpClient";

// VIOLATION of domain-purity: domain module imports from @/infrastructure/**,
// which matches neither @/domain/** nor @/shared/** in the allows clause.
export const fetchOrders = (userId: string) =>
  httpGet<unknown[]>(`/api/orders?userId=${userId}`);
