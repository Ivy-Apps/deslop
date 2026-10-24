// VIOLATION: SettingsScreen transitively imports SettingsContainer but
// SettingsContainer does not import SettingsStateEvent, so
// SettingsStateEvent is never reachable from SettingsScreen.
import { SettingsContainer } from '@/features/settings/SettingsContainer';

export function SettingsScreen() {
  return null;
}
