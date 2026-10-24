import { useState } from 'react';

export interface UserAuthState {
  isAuthenticated: boolean;
  isLoading: boolean;
  error: string | null;
}

export type UserAuthEvent =
  | { type: 'LOGIN'; username: string; password: string }
  | { type: 'LOGOUT' };

export function useUserAuthViewModel() {
  const [state, setState] = useState<UserAuthState>({
    isAuthenticated: false,
    isLoading: false,
    error: null,
  });

  const onEvent = async (event: UserAuthEvent) => {
    switch (event.type) {
      case 'LOGIN':
        setState(prev => ({ ...prev, isLoading: true, error: null }));
        // delegate to auth service
        setState({ isAuthenticated: true, isLoading: false, error: null });
        break;
      case 'LOGOUT':
        setState({ isAuthenticated: false, isLoading: false, error: null });
        break;
    }
  };

  return { state, onEvent };
}
