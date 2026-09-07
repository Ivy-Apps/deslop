// A barrel: it imports nothing and re-exports everything. Its dependencies
// are re-export edges, and without them nothing downstream of it is reachable.
export * from "@/features/home/home-service";
export { formatTitle as title } from "@/features/home/home-view";
export type { HomeProps } from "@/features/home/home-view";
