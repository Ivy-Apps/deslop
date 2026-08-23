// Conforms: a three-word variable, captured between 'use' and 'ViewModel' and
// reused as the kebab-case folder and the PascalCase use case.
import { ArchiveOrderUseCase } from '@/application/archive-order/ArchiveOrderUseCase';

export function useArchiveOrderViewModel() {
  return ArchiveOrderUseCase();
}
