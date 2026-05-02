export type UserDashboardUiState = {
  isLoading: boolean;
  items: string[];
};

export type UserDashboardUiEvent =
  | { type: 'LOAD' }
  | { type: 'RESET' };
