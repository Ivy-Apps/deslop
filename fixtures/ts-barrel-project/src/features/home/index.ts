// A barrel: it imports nothing and re-exports everything. Each form below is a
// separate edge, and each reaches a different module under @/server, so the
// rulebook can prove every one of them independently.
export * from "@/features/home/home-service";
export * as widgets from "@/features/home/home-widgets";
export { formatTitle as title } from "@/features/home/home-view";
export type { HomeProps } from "@/features/home/home-types";
