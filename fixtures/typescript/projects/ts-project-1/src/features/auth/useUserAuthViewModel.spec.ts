import { renderHook, act } from '@testing-library/react';
import { useUserAuthViewModel } from '@/features/auth/useUserAuthViewModel';

describe('useUserAuthViewModel', () => {
  it('starts unauthenticated', () => {
    const { result } = renderHook(() => useUserAuthViewModel());
    expect(result.current.state.isAuthenticated).toBe(false);
  });

  it('handles LOGOUT event', async () => {
    const { result } = renderHook(() => useUserAuthViewModel());
    await act(async () => {
      await result.current.onEvent({ type: 'LOGOUT' });
    });
    expect(result.current.state.isAuthenticated).toBe(false);
  });
});
