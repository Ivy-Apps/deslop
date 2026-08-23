// Violates nested-view-uses-its-shared-formatter, and only because of its
// depth: {{TARGET_DIR}} is this file's own directory, so the clause
// {{TARGET_DIR}}/../shared/format-money names
// @/components/stripe-connect/payment/shared/format-money here, while the same
// clause names @/components/stripe-connect/shared/format-money for a View one
// directory up. That module does not exist, and this file does not import it.
import { STRIPE_CONNECT } from '@/config/stripe-connect';

export function BadgeView() {
  return STRIPE_CONNECT.label;
}
