// The control for the acronym case. 'db-sync-job' and 'DBSyncJob' are one
// three-word name, so the rule applies - and this ViewModel drives the wrong
// use case, so it is reported. Note the message asks for 'DBSyncJobUseCase',
// the spelling actually captured, not the canonical 'DbSyncJob'.
import { ArchiveOrderUseCase } from '@/application/archive-order/ArchiveOrderUseCase';

export function useDBSyncJobViewModel() {
  return ArchiveOrderUseCase();
}
