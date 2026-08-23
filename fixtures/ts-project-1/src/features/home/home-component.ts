import { formatDate } from "../../lib/util";

export const getHomeData = async () => {
  const module = await import ('../heavy-module');
  const extras = await import ('../../lib/extra').extras;

  return {
    title: "Welcome Home",
    lastUpdated: formatDate(new Date()),
  };
};