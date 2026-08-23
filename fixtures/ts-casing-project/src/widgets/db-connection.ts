// Violates widgets-keep-out-of-internal. The folder is 'DBConnection' but the
// canonical PascalCase spelling of 'db-connection' is 'DbConnection', so only
// a forbidding clause - which accepts every spelling - catches this.
import { secret } from '@/internal/DBConnection/secret';

export const dbConnection = secret;
