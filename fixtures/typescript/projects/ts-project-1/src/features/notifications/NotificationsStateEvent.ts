export type NotificationsUiState = {
  isLoading: boolean;
  items: string[];
};

export type NotificationsUiEvent =
  | { type: 'LOAD' }
  | { type: 'DISMISS'; id: string };
