// Not a target of view-is-named-after-its-provider: 'stripe-connect' and
// 'Paypal' are not two spellings of one name, so the repeated variable does
// not bind and the rule correctly does not apply here.
export function PaypalView() {
  return 'paypal';
}
