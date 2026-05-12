// Infrastructure layer — HTTP adapter. Should never be imported by domain modules.
export const httpGet = async <T>(url: string): Promise<T> => {
  const res = await fetch(url);
  return res.json() as Promise<T>;
};
