// A barrel that re-exports something, but not its own View. It exists so that
// `barrel-exposes-its-view` is seen to fire: a rule whose target never matched
// would pass just as quietly as one that was satisfied.
export * from "@/features/settings/settings-service";
