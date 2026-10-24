// Violates view-has-a-storybook: there is no TransferView.stories file.
// Also violates view-may-reach-its-providers-shared-folder by importing a
// sibling SERVICE folder. {{TARGET_DIR}}/../shared/** reaches the provider's
// shared folder and nothing else, so payment/ is out of reach even though
// providers-are-isolated permits it - the .. is what draws that line.
import { useTransferViewModel } from '@/components/stripe-connect/payout/useTransferViewModel';
import { CheckoutView } from '@/components/stripe-connect/payment/CheckoutView';
import { formatMoney } from '@/components/stripe-connect/shared/format-money';
import { STRIPE_CONNECT } from '@/config/stripe-connect';

export function TransferView() {
  const { state } = useTransferViewModel();
  return CheckoutView() + formatMoney(state.amount);
}
