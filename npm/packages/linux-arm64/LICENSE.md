# TERMS OF SERVICE AND END USER LICENSE AGREEMENT (EULA)

This Terms of Service and End User License Agreement ("Agreement") is a legally binding contract between you (either an individual or a single legal entity) ("User" or "Licensee") and Ivy Apps Ltd ("Licensor", "we", or "us"), a company registered and operating under the laws of Bulgaria.

This Agreement governs your access to and use of the deslop.dev website, your user account, our hosted authentication and license verification infrastructure (collectively, the "Services"), and your downloading, installation, or use of the Deslop command-line interface linter tool software binary, including its updates, scripts, and documentation (the "Software").

By creating an account, completing a transaction via Polar.sh, downloading, installing, or using the Software, you explicitly agree to be bound by the terms of this Agreement. If you do not agree to the terms of this Agreement, do not access the Services or install or use the Software.

### 1. DEFINITIONS
* **"Software"** refers to the Deslop command-line interface (CLI) tool, including its executable binaries, associated scripts, documentation, and any updates or patches provided by the Licensor.
* **"Local Development Environment"** means a personal physical or virtual workstation where a human operator is directly and physically interacting with the Software via a standard terminal emulator.
* **"Automated Environment"** means any headless, unattended, or script-driven environment. This includes, but is not limited to, Continuous Integration (CI) systems, Continuous Deployment (CD) pipelines (e.g., GitHub Actions, GitLab CI, Jenkins, CircleCI), automated testing servers, or any environment where standard inputs and outputs are piped, mocked, or automated.
* **"Commercial License"** means a paid subscription or license key legitimately obtained from the Licensor via our authorized billing provider, Polar.sh. A Commercial License grants rights beyond the Free Tier, subject to the specific usage quotas, features, and billing terms associated with the subscription tier purchased.

### 2. SERVICES, USER ACCOUNTS, AND BILLING
1. **Account Responsibility:** To use certain features or validate licenses, you must create an account. You are solely responsible for maintaining the confidentiality of your web credentials and active sessions. Any actions taken under your account will be deemed conclusively as authorized by you.
2. **Third-Party Payment Processing:** All commercial subscriptions, transactions, and billing actions are handled exclusively by our third-party merchant provider, Polar.sh. We do not store or process your financial accounts or credit cards directly.
3. **EU Right of Withdrawal Waiver:** By purchasing a Commercial License, creating an account, or logging into the Software, you explicitly request immediate performance of this Agreement. You acknowledge and agree that you lose your statutory 14-day right of withdrawal under European Union consumer protection laws once the digital content delivery or performance begins.

### 3. GRANT OF LICENSE & RESTRICTIONS (FREE TIER)
1. **Grant of Free License:** Subject to your continuous compliance with this Agreement, the Licensor grants you a limited, personal, non-exclusive, non-transferable, and revocable license to use the Software free of charge **strictly within a Local Development Environment**.
2. **CI/CD Paywall:** You may **not** use the Free Tier of the Software in any Automated Environment. To run, execute, or integrate the Software within an Automated Environment, you must purchase and maintain a valid Commercial License.

### 4. COMMERCIAL LICENSES AND USAGE LIMITS
If you operate the Software under a Commercial License, your usage is governed by the parameters of your specific subscription tier.
1. **Dynamic Quotas and Features:** Specific pricing, usage limits (e.g., a maximum number of monthly CLI executions), and feature access are maintained on the Licensor's official website and authorized billing providers. These terms are incorporated by reference and may be updated by the Licensor.
2. **Overage:** Exceeding your allotted usage limits may result in the Software returning warnings, delaying execution, or halting processes entirely until the quota resets or the subscription is upgraded.
3. **Metric Manipulation:** Attempting to bypass, spoof, or manipulate usage metrics, counters, or environment variables to artificially deflate your recorded usage is a material breach of this Agreement.
4. **License Key Confidentiality and Responsibility:** The Licensee is solely responsible for maintaining the absolute security and confidentiality of their commercial license key. The Licensee must actively safeguard the key against unauthorized third-party access, leaks, or public exposure (including, without limitation, committing or pushing keys to public source control repositories such as GitHub). Any and all software executions or quota consumptions registered against the key on the Licensor's validation infrastructure shall be deemed conclusively as authorized usage by the Licensee.
5. **Credential Compromise and Key Remediation:** In the event of a credential leak, compromise, or premature exhaustion of quota by unauthorized third parties, the primary mechanism to instantly restore service and obtain a fresh license key is for the Licensee to purchase a new commercial subscription. Any manual technical intervention, key replacement, or quota resetting by the Licensor is governed strictly by Section 5 of this Agreement.

### 5. NO SUPPORT, MAINTENANCE, OR SERVICE LEVEL AGREEMENT (SLA)
1. **Self-Service and Self-Support:** The Software and Services are provided strictly as a self-service, self-support tool. The Licensor is under no legal or contractual obligation to provide the Licensee with any technical support, maintenance, customer service, updates, modifications, patches, or bug fixes.
2. **No SLA:** The Licensor does not provide any Service Level Agreement (SLA), uptime guarantee, operational continuity assurance, or guaranteed response-time windows.
3. **Discretionary Assistance:** The Licensee acknowledges that the Licensor operates as a sole practitioner. Any support interactions, bug resolutions, or assistance provided by the Licensor to the Licensee via email, community forums, or other communication channels are done entirely at the Licensor's sole discretion. The provision of occasional assistance does not create a habit, course of dealing, or any ongoing legal obligation to provide future support or maintenance.

### 6. TECHNICAL ENFORCEMENT AND PROHIBITED ACTIONS
The Software and Services contain technical mechanisms designed to detect Prohibited Uses and enforce quotas (e.g., detecting Automated Environments and prompting for human verification).

You expressly agree that you will not:
1. Attempt to bypass, disable, spoof, or circumvent any technical enforcement, environment variable checks, or human-verification prompts (CAPTCHAs) built into the Software or validation endpoints.
2. Use wrappers, pseudo-terminals (PTYs), or automated scripts to simulate a Local Development Environment in an attempt to evade the Commercial License requirement.
3. Reverse-engineer, decompile, disassemble, or modify the compiled binary of the Software.

Any attempt to bypass these mechanisms constitutes a material breach of this Agreement and terminates your license immediately. The Licensor reserves the right to implement functionality that delays, halts, or restricts execution if an unverified Automated Environment or license exploit is detected.

### 7. TELEMETRY AND LICENSE VALIDATION
The Software may transmit basic telemetry, environment data, run counts, or license keys to Licensor's servers strictly for the purpose of license validation, usage tracking, and enforcing the terms of this Agreement. You acknowledge and agree to this data transmission. The Licensor disclaims any liability for the accidental exposure of sensitive data or secrets resulting from attempts to bypass, intercept, or manipulate the Software's environment checks.

### 8. OWNERSHIP AND INTELLECTUAL PROPERTY
The Software and Services are licensed, not sold. Ivy Apps Ltd retains all rights, title, and interest in and to the Software and associated web infrastructure, including all copyrights, patents, trade secrets, trademarks, and other intellectual property rights therein. This Agreement does not grant you any rights to trademarks or service marks of Ivy Apps Ltd. All rights not expressly granted to you in this Agreement are reserved by the Licensor.

### 9. THIRD-PARTY COMPONENTS
The Software may include third-party components subject to their own open-source licenses. By using the Software, you also agree to comply with the terms and conditions of those third-party licenses.

### 10. DISCLAIMER OF WARRANTIES
THE SOFTWARE AND SERVICES ARE PROVIDED "AS IS" AND "WITH ALL FAULTS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, AND NON-INFRINGEMENT. THE LICENSOR DOES NOT WARRANT THAT THE SOFTWARE OR SERVICES ARE FREE OF DEFECTS, ERRORS, OR SECURITY VULNERABILITIES, THAT THEY WILL OPERATE WITHOUT INTERRUPTION, OR THAT THE LICENSE VALIDATION SERVERS WILL BE AVAILABLE AT ALL TIMES. THE ENTIRE RISK ARISING OUT OF THE USE OR PERFORMANCE OF THE SOFTWARE AND SERVICES REMAINS WITH YOU.

### 11. LIMITATION OF LIABILITY
IN NO EVENT SHALL IVY APPS LTD, ITS DIRECTORS, EMPLOYEES, OR ITS SUPPLIERS BE LIABLE FOR ANY DAMAGES WHATSOEVER (INCLUDING, WITHOUT LIMITATION, INCIDENTAL, DIRECT, INDIRECT, SPECIAL, OR CONSEQUENTIAL DAMAGES, DAMAGES FOR LOSS OF BUSINESS PROFITS, BUSINESS INTERRUPTION, LOSS OF BUSINESS INFORMATION, COMPUTING COSTS, DATA LOSS, OR OTHER PECUNIARY LOSS) ARISING OUT OF THE USE OR INABILITY TO USE THE SOFTWARE OR SERVICES, EVEN IF THE LICENSOR HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

IN NO EVENT WILL THE LICENSOR'S TOTAL MAXIMUM AGGREGATE LIABILITY TO YOU FOR ALL DAMAGES, CLAIMS, AND CAUSES OF ACTION EXCEED THE AMOUNT ACTUALLY PAID BY YOU FOR THE SOFTWARE OR SERVICE UNDER THIS AGREEMENT IN THE TWELVE (12) MONTHS PRECEDING THE CLAIM. BECAUSE SOME STATES/JURISDICTIONS DO NOT ALLOW THE EXCLUSION OR LIMITATION OF LIABILITY FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES, THE ABOVE LIMITATION MAY NOT APPLY TO YOU.

### 12. INDEMNIFICATION
You agree to indemnify, defend, and hold harmless Ivy Apps Ltd and its officers, directors, and employees from and against any claims, damages, liabilities, losses, and expenses (including reasonable attorneys' fees) arising out of or related to your use of the Software or Services, your violation of this Agreement, or your violation of any rights of a third party.

### 13. TERMINATION
This Agreement is effective until terminated. Your rights under this Agreement will terminate automatically without notice from the Licensor if you fail to comply with any term(s) of this Agreement. Upon termination, you must cease all use of the Software and Services and destroy all copies, full or partial, of the Software.

### 14. GENERAL AND GOVERNING LAW
This Agreement constitutes the entire agreement between the parties concerning the subject matter hereof. If any provision of this Agreement is held to be invalid or unenforceable, the remaining provisions shall remain in full force and effect.

This Agreement shall be governed by and construed in accordance with the laws of Bulgaria, without regard to its conflict of laws rules. Any disputes arising under or in connection with this Agreement shall be subject to the exclusive jurisdiction of the competent courts located in Sofia, Bulgaria.
