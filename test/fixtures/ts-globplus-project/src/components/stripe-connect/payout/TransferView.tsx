// Violates view-has-a-storybook: there is no TransferView.stories file.
import { useTransferViewModel } from '@/components/stripe-connect/payout/useTransferViewModel';
import { STRIPE_CONNECT } from '@/config/stripe-connect';

export function TransferView() {
  const { state } = useTransferViewModel();
  return state.amount;
}
