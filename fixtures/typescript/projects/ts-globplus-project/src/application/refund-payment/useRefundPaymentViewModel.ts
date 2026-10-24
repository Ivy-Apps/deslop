// Violates view-model-drives-its-use-case: reaches for another use case. Only
// correct capture of all three words tells RefundPayment from ArchiveOrder.
import { ArchiveOrderUseCase } from '@/application/archive-order/ArchiveOrderUseCase';

export function useRefundPaymentViewModel() {
  return ArchiveOrderUseCase();
}
