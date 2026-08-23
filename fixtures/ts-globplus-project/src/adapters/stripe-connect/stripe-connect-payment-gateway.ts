// Violates adapter-imports-its-provider-config: never imports
// @/config/stripe-connect.
//
// The file segment reads "stripe-connect-payment-gateway". Split greedily it
// would give provider-name = "stripe-connect-payment", which the folder above
// has already ruled out - so the provider is "stripe-connect" and the service
// type is "payment-gateway", and the rule demands that provider's config.
export const gateway = {
  capture: (amount: number) => amount,
};
