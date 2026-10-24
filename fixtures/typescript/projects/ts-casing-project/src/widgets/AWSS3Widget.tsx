// A DOCUMENTED LIMITATION, pinned on purpose. 'AWSS3' does not say where 'aws'
// ends and 's3' begins, so the derived kebab-case is 'awss3' and the rule
// demands '@/config/awss3', which cannot exist. Reported as a violation; a
// project that hits this baselines it or names the folder in the target too,
// as @/components/{{provider-name}}/{{ProviderName}}View does.
import { config } from '@/config/aws-s3';

export function AWSS3Widget() {
  return config.name;
}
