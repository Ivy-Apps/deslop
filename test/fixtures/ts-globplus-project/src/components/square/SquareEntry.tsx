// The control for provider-entry-is-named-after-its-folder. 'square' and
// 'Square' are one name and the ** stands for zero directories, so the rule
// applies here - and there is no @/config/square, so it is reported. While the
// globstar's text was being read as the provider, this rule matched nothing at
// all and its silence was indistinguishable from success.
export function SquareEntry() {
  return 'square';
}
