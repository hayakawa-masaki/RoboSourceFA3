1 ' ===================================
2 '
3 '  21050001 STEP5 Assy2�v���O����
4 '
5 ' �쐬�ҁFM.Hayakawa
6 ' �쐬���F2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1���痬�p
8 ' Ver 0.3 2021.12.22 �摜�����֐�ISInspection��ISInspectionSingle�A�摜�����ǉ� file:210542003
9 ' ===================================
10 '===== <Insight�萔> =====
11 '===== <Insight�ϐ���`> =====
12 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
13 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
14 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
15 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
16 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
17 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
18 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
19 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
20 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
21 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
22 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
23 '
24 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
25 'Output Signal
26 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
27 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
28 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
29 '
30 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
31 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
32 '
33 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
34 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
35 '��Ɨp�ϐ�
36 Def Inte MInspErrNum                '�������s�G���[�ԍ�
37 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
38 Def Inte MRtn                       'Function�߂�l�擾�p
39 Def Inte MRtn2                      'Function�߂�l�擾�p
40 Def Inte MRet3                      'Function�߂�l�擾�p
41 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
42 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
43 Def Inte MOvrdA                     '�l�W����Ovrd �ϗp
44 Def Float MSpdA                     '�l�W����Spd�@�ϗp
45 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
46 '===== <Insight�ϐ��ݒ�> =====
47 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
48 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
49 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
50 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
51 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
52 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
53 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
54 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
55 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
56 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
57 'Output Signal
58 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
59 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
60 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
61 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
62 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
63 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
64 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
65 '===== <�d�h���ϐ���`> =====
66 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
67 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
68 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
69 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
70 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
71 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
72 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
73 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
74 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
75 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
76 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
77 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
78 Y60_Driver=12240 '�d�h�������v��� CCW
79 Y61_Driver=12241 '�d�h�����v��� CW
80 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
81 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
82 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
83 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
84 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
85 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
86 X34_ScrewReady1=11259 '�˂�����1�@Read
87 '===== <�d�h���萔> =====
88 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
89 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
90 Dim PEscapePosi(10)
91 MLoopCnt% = 0'
92 '===== <���{�b�g�萔> =====
93 '===== <���{�b�g�ϐ���`> =====
94 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
95 MCommentD1001 = 0
96 MCommentD1002 = 0
97 MCommentD1003 = 0
98 MScreenNo = 0
99 '
100 MCommentTSU = 0
101 MCommentTSD = 0
102 '�E�B���h��ʔԍ��ݒ�
103 MWindReSet = 0
104 MWindInfoScr = 5
105 MWindErrScr = 10
106 MWindErrScr2 = 11
107 MWindErrScr3 = 13
108 MWindErrScr17 = 17
109 MWindErrScr18 = 18
110 MWindCmmnScr = 20
111 MWindJigRelase19049 = 60
112 MWindJigRelase19050 = 61
113 MWindJigRelase19051 = 62
114 '
115 MClear% = 0        'KEY_�̃N���A
116 MAbout% = 1        'KEY_��~
117 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
118 MContinue% = 3     'KEY_�p�� �ēx����������s��
119 '
120 Def Inte MNgProcess
121 MNgProcess% = 5      'KEY_NG
122 '
123 MAssyOK% = 6       '�g������
124 MPass% = 7         '�H���p�X
125 MPiasNG% = 8       'Pias�m�F������NG
126 '
127 '�������pKEY�ԍ�   '
128 MRobotInit1% = 11  '�����ʒu�p
129 MRobotInit2% = 12  '�����ʒu�p
130 MRobotInit3% = 13  '�����ʒu�p
131 MRobotInit4% = 14  '�����ʒu�p
132 '
133 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
134 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
135 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
136 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
137 '
138 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
139 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
140 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
141 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
142 '
143 MOK% = 1               '�e����p
144 MNG% = 0               '�e����p
145 MTIMEOUT% = -1         '�e����p
146 MJudge% = 0            '������i�[�p
147 '
148 MRECIVETIME& = 0
149 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
150 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
151 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
152 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
153 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
154 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
155 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
156 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
157 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
158 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
159 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
160 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
161 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
162 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
163 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
164 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
165 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
166 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
167 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
168 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
169 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
170 MIN_PIAS_MyProcessComp% = 11573        '���H����������
171 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
172 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
173 '
174 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
175 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
176 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
177 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
178 '
179 MOUT_PiasAssyResultOK% = 12549    '�g��OK
180 MOUT_PiasAssyResultNG% = 12550    '�g��NG
181 MOUT_PiasAssyResultWr% = 12548    '�H��������������
182 '
183 MIN_PiasProcessNG% = 11559        '�H����������NG
184 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
185 MIN_PiasProcessOK% = 11558        '�H����������OK
186 '
187 MIN_Insight_Use% = 11369               '�摜�m�FON
188 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
189 '
190 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
191 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
192 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
193 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
194 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
195 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
196 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
197 '
198 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
199 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
200 '
201 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
202 '
203 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
204 '
205 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
206 MopeNo% = 0
207 MOvrdA% = 10
208 MRtn% = 0
209 MRet = 0
210 MRet3% = 0
211 '
212 Def Inte MInputQty          '������ ���Z�ϐ�
213 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
214 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
215 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
216 Def Inte nAssyOkQty         '���g�p
217 Def Inte MScrewNo
218 Def Inte MReTry
219 '===== <IO�ϐ���`> =====
220 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
221 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
222 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
223 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
224 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
225 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
226 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
227 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
228 '
229 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
230 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
231 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
232 '
233 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
234 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
235 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
236 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
237 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
238 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
239 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
240 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
241 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
242 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
243 '
244 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
245 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
246 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
247 '
248 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
249 '
250 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
251 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
252 '
253 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
254 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
255 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
256 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
257 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
258 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
259 '
260 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
261 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
262 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
263 '
264 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
265 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
266 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
267 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
268 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
269 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
270 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
271 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u '���l12250����12248�֕ύX(8/5����)
272 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
273 MOUT_VB1%   =  12250    ' �A�[����[�@�l�W�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
274 '
275 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
276 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
277 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
278 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
279 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
280 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
281 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
282 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
283 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
284 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
285 '
286 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
287 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
288 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
289 '
290 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
291 '
292 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
293 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
294 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
295 '
296 '����
297 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
298 Def Inte MOn                            '�o��=1
299 Def Inte MOff                           '�o��=0
300 '
301 '�˂����ߑ��u_�o�̓A�h���X
302 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
303 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
304 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
305 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
307 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
308 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
309 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
310 '�˂����ߑ��u_���̓A�h���X
311 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
312 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
313 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
314 Def Inte MIN_ScwT_Case1                 '����1��~����M
315 Def Inte MIN_ScwT_Case2                 '����2��~����M
316 Def Inte MIN_ScwT_Case3                 '����3��~����M
317 Def Inte MIN_ScwT_Case4                 '����4��~����M
318 Def Inte MIN_ScwT_Case5                 '����5��~����M
319 '
320 Def Inte MRetryLimit                    ' ���g���C��
321 Def Inte MRetryCount                    ' ���g���C�J�E���g
322 '
323 Dim MScwT_Case1%(2)               '����1��~�ϐ�
324 Dim MScwT_Case2%(2)               '����2��~�ϐ�
325 Dim MScwT_Case3%(2)               '����3��~�ϐ�
326 Dim MScwT_Case4%(2)               '����4��~�ϐ�
327 Dim MScwT_Case5%(2)               '����5��~�ϐ�
328 '
329 Def Pos PActive                     '�������W�n �ʒu�ϐ� ���݈ʒu
330 Def Pos Pmove                       '�������W�n �ʒu�ϐ� �ړ���
331 Def Inte MRecoveryPass              '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s'
332 '����
333 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
334 MOn% = 1                                 '�o�� = 1
335 MOff% = 0                                '�o�� = 0
336 '
337 '�˂����ߋ@_�A�h���X�ݒ�
338 MOUT_ScwT_ComChk% = 12816               '�ʐM�m�F���M
339 MOUT_ScwT_ST% = 12849                   '�˂����ߊJ�n�𑗐M
340 MOUT_ScwT_ReSTOK% = 12850               '�ĊJ�n��M�𑗐M
341 MOUT_ScwT_FinOK% = 12852                '�˂����ߊ�����M�𑗐M
342 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
343 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
344 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
345 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
346 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
347 '
348 MIN_ScwT_comOK% = 11824                 '�˂����ߑ��u����ԐM
349 MIN_ScwT_STRec% = 11857                 '�˂����ߊJ�n����M
350 MIN_ScwT_ReST% = 11858                  '�ĊJ�n����M
351 MIN_ScwT_Fin% = 11860                   '�˂����ߊ�������M
352 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
353 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
354 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
355 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
356 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
357 '
358 MScwT_Case1%(1) = MIN_ScwT_Case1%
359 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
360 MScwT_Case2%(1) = MIN_ScwT_Case2%
361 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
362 MScwT_Case3%(1) = MIN_ScwT_Case3%
363 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
364 MScwT_Case4%(1) = MIN_ScwT_Case4%
365 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
366 MScwT_Case5%(1) = MIN_ScwT_Case5%
367 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
368 '
369 MRetryLimit% = 2
370 '
371 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
372 Function M% fnAssyStart
373     M_20# = MClear%                       '������
374 '
375 '�g�ݗ��ĊJ�n
376     Ovrd 100
377 '    Mov PInitialPosition   '�b��R�����g�A�E�g
378     '�`�P�b�gID��ǂ�
379 '    Mvs PTicketRead             'ID�ǂ݈ʒu
380     '�����ʒu��ݒ�
381     PTemp = P_Curr
382     MRtn = 0
383     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
384         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
385             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
386                 MRtn = 1
387                 Break
388             EndIf
389             Break
390         EndIf
391         Break
392     EndIf
393     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
394     If MRtn = 1 Then
395         M_Out(12262) = 1            '�ʒu���ߏoON
396         Mov PTicketRead
397         Break
398     Else
399         Mov PInitialPosition
400         M_Out(12262) = 1            '�ʒu���ߏoON
401         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
402         Mvs PTicketRead             'ID�ǂ݈ʒu
403         Break
404     EndIf
405 '
406 '---------------------------------------------------------------
407     '���͐ݒ�(�ሳ)22/08/09����
408     M_Out(12266) = 1
409     M_Out(12267) = 0
410 '---------------------------------------------------------------
411 '
412 '
413     MRtn = 1                        'MRtn������
414     *RE_TICKET_READ
415 '    MRtn = fnPiasCheck()               'ID�ǂݎ��
416 '    PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
417 '    MInspGroup%(1) = 1              '����G�ԍ�
418 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
419     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
420         MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
421         '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
422         '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
423     EndIf
424     If MRtn = 1 Then GoTo *CompRead
425     Mvs PTicketRead_1                       ' ��U���ɑҔ� �ǉ� 22/07/16 M,H
426 '    fErrorProcess(11,111,254,0)
427     If M_20# = MPass% Then GoTo *AssyEnd    ' ���ւ������ꂽ���̃R�����g�����{�W�����v��ύX 2022/07/20 M.H
428     If M_20# = MNext% Then M_20# = MClear%
429     If M_20# = MAbout% Then GoTo *AssyEnd       ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
430     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
431     If M_20# = MContinue% Then GoTo *RE_TICKET_READ
432     GoTo *ASSY_ERROR_END
433     *CompRead
434     '
435     *INITIAL_CHECK
436     '�n���h�̏�Ԃ��C�j�V�����ɖ߂�
437     MRtn =frInCheck(11264,0,MSETTIMEOUT05&) 'PCB���o(����ƃG���[)
438     If MRtn = 1 Then GoTo *CompCheck_1
439     fErrorProcess(11,0,281,0)
440     If M_20# = MNext% Then M_20# = MClear%
441     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
442     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
443     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
444     *CompCheck_1
445     '
446     If M_In(11266) = 1 Then
447         M_Out(12256) = 0        'PCB�`���b�N�JOFF
448         M_Out(12257) = 1        'PCB�`���b�N��ON
449         Break
450     EndIf
451     If M_In(11268) = 1 Then
452         M_Out(12258) = 0        'PCB�V�����_�[�oOFF
453         M_Out(12259) = 1        'PCB�V�����_�[��ON
454         Break
455     EndIf
456     If M_In(11270) = 1 Then
457         M_Out(12260) = 0        'BtoB�V�����_�[�oOFF
458         M_Out(12261) = 1        'BtoB�V�����_�[��ON
459         Break
460     EndIf
461     '
462     MRtn =frInCheck(11265,1,MSETTIMEOUT05&) 'PCB�`���b�N���o
463     If MRtn = 1 Then GoTo *CompCheck_2
464     fErrorProcess(11,240,281,0)
465     If M_20# = MNext% Then M_20# = MClear%
466     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
467     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
468     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
469     *CompCheck_2
470     '
471     MRtn =frInCheck(11267,1,MSETTIMEOUT05&) 'PCB�V�����_�[�ߌ��o
472      If MRtn = 1 Then GoTo *CompCheck_3
473     fErrorProcess(11,239,281,0)
474     If M_20# = MNext% Then M_20# = MClear%
475     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
476     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
477     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
478     *CompCheck_3
479     '
480     MRtn =frInCheck(11269,1,MSETTIMEOUT05&) 'BtoB�V�����_�[�ߌ��o
481     If MRtn = 1 Then GoTo *CompCheck_4
482     fErrorProcess(11,243,281,0)
483     If M_20# = MNext% Then M_20# = MClear%
484     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
485     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
486     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
487     *CompCheck_4
488     '
489     '���i�ʒu����
490     *RE_POSITIONING        '�ʒu���߃��g���C�p
491     M_Out(12262)=1 Dly 0.3      '�ʒu���߃p���X�M��
492     'Wait M_In(11273)=1          '�ʒu���ߏo�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
493     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
494     If MRtn = 1 Then GoTo *CompPosition_1
495     fErrorProcess(11,231,282,0)
496     If M_20# = MNext% Then M_20# = MClear%
497     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
498     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
499     If M_20# = MContinue% Then GoTo *RE_POSITIONING
500     *CompPosition_1
501     '
502     Dly 0.5
503     M_Out(12264)=1 Dly 0.3      '�v�b�V���p���X�M��
504     'Wait M_In(11275)=1          '�v�b�V���o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
505     MRtn = frInCheck(11275,1,MSETTIMEOUT05&)    '�v�b�V���ʒu�o�[���o(8/26����)
506     If MRtn = 1 Then GoTo *CompPosition_2
507     fErrorProcess(11,232,282,0)
508     If M_20# = MNext% Then M_20# = MClear%
509     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
510     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
511     If M_20# = MContinue% Then GoTo *RE_POSITIONING
512     *CompPosition_2
513     '
514 '    '�ySOC���ID�ǂݍ��݁z
515 '    *RE_SOC_CHECK1
516 '    PInspPosition(1) = PSocPcbRead  'SOC���ID�ǎ�ʒu
517 '    MInspGroup%(1) = 2              '����G�ԍ�
518 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
519 '    If MRtn = 1 Then
520 '        M_Out(12571) = 1        '��ԍ��R�s�[1ON
521 '        Dly 0.3
522 '        M_Out(12566) = 1        '��ԍ��R�s�[�v��ON
523 '        Wait M_In(11581) = 1    '��ԍ��R�s�[����
524 '        M_Out(12571) = 0        '��ԍ��R�s�[1OFF
525 '        M_Out(12566) = 0        '��ԍ��R�s�[�v��OFF
526 '        Dly 0.3
527 '        M_Out(12557)= 1 Dly 0.3         ' ��ԍ��ƍ��r�b�gON
528 '    EndIf
529 '    If MRtn = 1 Then GoTo *CompSocCheck1
530 '    fErrorProcess(11,97,25,0)
531 '    If M_20# = MNext% Then M_20# = MClear%
532 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
533 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
534 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
535 '    *CompSocCheck1
536 '    'SOC���ID��ǂ�
537 ''    Mov PSocPcbRead             'ID�ǂ݈ʒu
538 '    '
539     'SOC������
540     *RE_GET_SOC
541     '
542     Mov PSocGet_2               '��s�b�N�A�b�v���_  Y:�ύX 107.160��106.160
543     M_Out(12256)=0              '��`���b�N�JOFF
544     M_Out(12257)=1              '��`���b�N��ON
545     '
546     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    '�`���b�N�Z���T�[ON
547     If MRtn = 1 Then GoTo *CompGetSOC_1
548     fErrorProcess(11,240,284,0)
549     If M_20# = MNext% Then M_20# = MClear%
550     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
551     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
552     If M_20# = MContinue% Then GoTo *RE_GET_SOC
553     *CompGetSOC_1
554     '
555     M_Out(12259)=0              'PCB�V�����_�[��OFF
556     M_Out(12258)=1              'PCB�V�����_�[�oON
557     Dly 0.2
558     '
559 '    Wait M_In(11268)=1          'PCB�V�����_�[�o�[�Z���T�[ON
560     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)
561     If MRtn = 1 Then GoTo *CompGetSOC_2
562     fErrorProcess(11,238,284,0)
563     If M_20# = MNext% Then M_20# = MClear%
564     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
565     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
566     If M_20# = MContinue% Then GoTo *RE_GET_SOC
567     *CompGetSOC_2
568     '
569     Mov PSocGet_1               '���� Y:�ύX 107.160��106.160
570     Ovrd 40
571     Mvs PSocGet                 '��s�b�N�A�b�v�ʒu  Y:�ύX 107.170��106.170
572     Dly 0.3
573     Ovrd 5                      '2021-12-19�ǉ� AJ
574     '
575     '
576 '    Wait M_In(11264)=1          'PCB���o�Z���T�[ON
577     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
578     If MRtn = 1 Then GoTo *CompGetSOC_3
579     fErrorProcess(11,0,284,0)
580     If M_20# = MNext% Then M_20# = MClear%
581     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
582     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
583     If M_20# = MContinue% Then GoTo *RE_GET_SOC
584     *CompGetSOC_3
585     '
586     M_Out(12257)=0              '��`���b�N��OFF
587     M_Out(12256)=1              '��`���b�N�JON
588     Dly 0.2                     '�c���͊m�ۗp�f�B���C(22/09/30����)
589     '
590 '    Wait M_In(11266)=1          '�`���b�N�J�Z���T�[ON
591     MRtn = frInCheck(11266 , 1 , MSETTIMEOUT05&)
592     Mvs PSocGet_1               '����  Y:�ύX 107.160��106.160
593     If MRtn = 1 Then GoTo *CompGetSOC_4
594     Mov PSocGet_2               ' Y:�ύX 107.160��106.160
595     fErrorProcess(11,241,284,0)
596     If M_20# = MNext% Then M_20# = MClear%
597     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
598     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
599     If M_20# = MContinue% Then GoTo *RE_GET_SOC
600     *CompGetSOC_4
601     '
602     'Wait M_In(11264)=1          'PCB���o�Z���T�[ON
603     '
604     '���L�APCB����ƃG���[�����ǉ� 2021-12-19 AJ
605     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
606     If MRtn = 1 Then GoTo *CompGetSOC_41
607     fErrorProcess(11,0,284,0)
608     If M_20# = MNext% Then M_20# = MClear%
609     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
610     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
611     If M_20# = MContinue% Then GoTo *RE_GET_SOC
612     *CompGetSOC_41
613     '
614     '
615     'Ovrd 100                   '�ʒu�ύX2021-12-19�ǉ�AJ
616     Ovrd 15                     '�ǉ�1/17����
617     Mov PSocGet_2               '��s�b�N�A�b�v���_
618     Ovrd 100                    '2021-12-19�ǉ�AJ
619     '
620     'SOC��𐻕i��ɒu��
621     Mov PSocSet_2               '��u�����_
622     Mov PSocSet_1               '���i���
623     Dly 0.1
624     Ovrd 40
625     Mvs PSocSet                 '��u���ʒu�i�󒆂ŗ����j
626     M_Out(12256)=0              '��`���b�N�JOFF
627     M_Out(12257)=1              '��`���b�N��ON
628 '
629     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
630     Mvs PSocSet_1               '���i���
631     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
632 '
633     'Wait M_In(11265)=1          '�`���b�N�Z���T�[ON
634     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)
635     If MRtn = 1 Then GoTo *CompGetSOC_5
636     fErrorProcess(11,240,284,0)
637     If M_20# = MNext% Then M_20# = MClear%
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_GET_SOC
641     *CompGetSOC_5
642     '
643 '    Wait M_In(11268)=1          'PCB�V�����_�[�o�[�Z���T�[ON�E�E�E����OFF���������}�������s���Ă���
644     MRtn = frInCheck(11268 , 1 , MSETTIMEOUT05&)
645     If MRtn = 1 Then GoTo *CompGetSOC_6
646     fErrorProcess(11,0,284,0)
647     If M_20# = MNext% Then M_20# = MClear%
648     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
649     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
650     If M_20# = MContinue% Then GoTo *RE_GET_SOC
651     *CompGetSOC_6
652     '
653     'Wait M_In(11264)=0          'PCB���o�Z���T�[OFF
654     M_Out(12258)=0              'PCB�V�����_�[�oOFF
655     M_Out(12259)=1              'PCB�V�����_�[��ON
656     '
657 '    Wait M_In(11267)=1          'PCB�V�����_�[�ߒ[�Z���T�[ON
658     MRtn = frInCheck(11267 , 1 , MSETTIMEOUT05&)
659     If MRtn = 1 Then GoTo *CompGetSOC_7
660     fErrorProcess(11,239,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
663     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
664     If M_20# = MContinue% Then GoTo *RE_GET_SOC
665     *CompGetSOC_7
666     Ovrd 100
667     Mov PSocSet_2               '��u�����_
668 '�ySOC���ID�ǂݍ��݁z
669     *RE_SOC_CHECK1
670     PInspPosition(1) = PSocPcbRead  'SOC���ID�ǎ�ʒu
671     MInspGroup%(1) = 2              '����G�ԍ�
672     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
673 '
674     If MRtn = 1 Then GoTo *CompSocCheck1
675     fErrorProcess(11,97,25,0)
676     If M_20# = MNext% Then M_20# = MClear%
677     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
678     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
679     If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
680     *CompSocCheck1
681 '�y���ID�R�s�[�z
682     *RE_PCB_RECORD
683     M_Out(12571) = 1    ' �̈�1 ��ԍ��R�s�[ (D2600-) On
684     Dly 0.1
685     M_Out(12566) = 1    ' toPLC_��ԍ��R�s�[�v�� On
686 '
687     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��R�s�[���� On
688     If MRtn = 1 Then
689         M_Out(12571) = 0  ' �̈�1 ��ԍ��R�s�[ (D2600-) Off
690         Dly 0.1
691         M_Out(12566) = 0  ' toPLC_��ԍ��R�s�[�v�� Off
692 '        GoTo *RE_PCB_COMPAIRE   ' ��ԍ��ƍ��ɃX�L�b�v
693     Else
694         fErrorProcess(11,39,25,0)
695         If M_20# = MNext% Then M_20# = MClear%
696         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
697         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
698         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
699     EndIf
700 '�y���ID�ƍ��i�R�t���j�z
701     MRetryCount% = 0
702     While (MRetryCount% <= MRetryLimit%)
703         *RE_PCB_COMPAIRE
704         M_Out(12557)= 1 ' ��ԍ��ƍ��r�b�gON
705         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��ƍ�OK(M420) On
706         If MRtn = 1 Then
707             M_Out(12557)= 0     ' ��ԍ��ƍ��r�b�gOff
708             ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
709             MRetryCount% = 99
710         Else
711             If MRetryCount% = MRetryLimit% Then
712                 If M_In(11565) = 1 Then
713                     fErrorProcess(11,37,25,0)
714                 Else
715                     fErrorProcess(11,38,25,0)
716                 EndIf
717                 If M_20# = MNext% Then
718                     M_20# = MClear%
719                     ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
720                     MRetryCount% = 99
721                 EndIf
722                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
723                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
724                 If M_20# = MContinue% Then
725                     MRetryCount% = 0
726                 EndIf
727             Else
728                 ' ���g���C�񐔃C���N�������g
729                 MRetryCount% = MRetryCount% + 1
730                 Dly 0.5 ' ���̍H���ƃ^�C�~���O�����炷�ׂ̃f�B���C
731             EndIf
732         EndIf
733     WEnd
734 '�ySoc��摜�`�F�b�N�z
735 '    *RE_SOC_CHECK2
736 '    PInspPosition(1) = PSocCheck    'Soc��摜�`�F�b�N�ʒu
737 '    MInspGroup%(1) = 3              '����G�ԍ�
738 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
739 '    If MRtn = 1 Then GoTo *CompSocCheck2
740 '    fErrorProcess(11,43,46,0)
741 '    If M_20# = MNext% Then M_20# = MClear%
742 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
743 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
744 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK2
745 '    *CompSocCheck2
746     '��u���ʒu�摜�����i�s�v�H�j
747     '
748     'SOC���BtoB�v���X
749     'Mov PSocPress_2             'BtoB�v���X���_
750     *RE_BtoBPRESS   'BtoB�V�����_�[���g���C
751     Mov PSocPress_1             '�v���X���
752     M_Out(12261)=0              '�v���X�V�����_�[��OFF
753     M_Out(12260)=1              '�v���X�V�����_�[�oON
754     Dly 0.2
755     '
756     'Wait M_In(11270)=1          '�v���X�V�����_�[�o�[�Z���T�[ON(�C���ɂ��R�����g�A�E�g(8/27����))
757     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    '�v���X�V�����_�[�o�[�Z���T�[ON(8/27����)
758     If MRtn = 1 Then GoTo *CompPress_1
759     fErrorProcess(11,242,284,0)
760     If M_20# = MNext% Then M_20# = MClear%
761     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
762     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
763     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
764     *CompPress_1
765 '
766 '--------------------------------------------
767     '���͌��o(�ሳ��)
768     '22/07/29�ǉ� ����
769 '--------------------------------------------
770 *RE_Pa_OUT
771     If M_20# = MContinue% Then
772     M_Out(12266) = 1
773     M_Out(12267) = 0
774     Dly 0.5
775     M_20# = MClear%
776     EndIf
777     MRtn = frInCheck(11277,1,MSETTIMEOUT05&)     'MDV�p���͌��o(22/07/29����)
778     MRtn2 = frInCheck(11278,0,MSETTIMEOUT05&)    'KA�p���͌��o(ON�ŏオ�肷��)(22/07/29����)
779     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompPaOut
780     If MRtn = 0 Then
781         fErrorProcess(11,200,201,0)
782     ElseIf MRtn2 = 0 Then
783         fErrorProcess(11,200,201,0)
784     EndIf
785     If M_20# = MNext% Then M_20# = MClear%
786     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
787     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
788     If M_20# = MContinue% Then GoTo *RE_Pa_OUT
789 *CompPaOut
790     '
791     '
792     Ovrd 40
793     Mvs PSocPress               '�v���X�G���h�[�܂ňړ�
794     Dly 0.5
795     'Wait M_In(11270)=1          '�v���X�V�����_�[�o�[�Z���T�[ON�c����OFF��������R�l�N�^�J�o�[�L
796     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    '�v���X�V�����_�[�o�[�Z���T�[ON(8/27����)
797     If MRtn = 1 Then GoTo *CompPress_2
798     Mvs PSocPress_1              '�v���X���
799     fErrorProcess(11,70,71,0)
800     If M_20# = MNext% Then M_20# = MClear%
801     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
802     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
803     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
804     *CompPress_2
805     '
806     'Dly 0.2
807     '
808     *RE_BtoB_REST
809     M_Out(12260)=0              '�v���X�V�����_�[�oOFF
810     M_Out(12261)=1              '�v���X�V�����_�[��ON
811 '    Wait M_In(11269)=1          '�v���X�V�����_�[�ߒ[�Z���T�[ON
812     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    '�v���X�V�����_�[�ߒ[�Z���T�[ON(8/27����)
813     If MRtn = 1 Then GoTo *CompBtoBRest
814     fErrorProcess(11,243,284,0)
815     If M_20# = MNext% Then M_20# = MClear%
816     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
817     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
818     If M_20# = MContinue% Then GoTo *RE_BtoB_REST
819     *CompBtoBRest
820     '
821     Ovrd 100
822     Mov PSocPress_1             '�v���X���
823     M_Out(12266) = 0
824     M_Out(12267) = 0
825     'Mov PSocPress_2            'BtoB�v���X���_
826     '
827 'Soc��l�W����
828     PGetScrewPos(1) = PScrewSupply_1        ' �˂��s�b�N�A�b�v������
829     PGetScrewPos(2) = PScrewSupply_2        ' �˂������@���_����
830     PGetScrewPos(9) = PScrewSupply_9        ' �l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
831     PGetScrewPos(10) = PScrewSupply         ' �˂��s�b�N�A�b�v������
832     '
833     'Soc��p�l�W�����@�փl�W�����ɍs��
834     *RE_SCREW_GET_1
835     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
836     If MRtn = 1 Then GoTo *CompScrewGet_1
837     If M_20# = MNext% Then M_20# = MClear%
838     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
839     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
840     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
841     *CompScrewGet_1
842     '
843     PScrewPos(1) = PScrewSoc1_1             ' �˂��s�b�N�A�b�v������
844     PScrewPos(2) = PScrewSoc1_0             ' �l�W1���ߊJ�n�ʒu����(10/8 M.H)
845     PScrewPos(10) = PScrewSoc1              ' �˂������@���_����
846     '�@�ԃl�W����
847     M_Out16(12672) = 1              '�l�W���߈ʒu�ԍ����M
848     MRtn = ScrewTight(PScrewPos,1,10.0)
849     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
850     If MRtn = 1 Then GoTo *CompScrew1
851     Mov PInitialPosition
852     fErrorProcess(11,53,52,0)
853     If M_20# = MNext% Then M_20# = MClear%
854     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
855     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
856     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
857     *CompScrew1
858     '
859     'Soc��p�l�W�����@�փl�W�����ɍs��
860     *RE_SCREW_GET_2
861     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
862     If MRtn = 1 Then GoTo *CompScrewGet_2
863     If M_20# = MNext% Then M_20# = MClear%
864     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
865     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
866     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
867     *CompScrewGet_2
868     '
869     PScrewPos(1) = PScrewSoc2_1             ' �˂��s�b�N�A�b�v������
870     PScrewPos(2) = PScrewSoc2_0             ' �l�W2���ߊJ�n�ʒu����(10/8 M.H)
871     PScrewPos(10) = PScrewSoc2              ' �˂������@���_����
872     '�A�ԃl�W����
873     M_Out16(12672) = 2              '�l�W���߈ʒu�ԍ����M
874     MRtn = ScrewTight(PScrewPos,1,10.0)
875     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
876     If MRtn = 1 Then GoTo *CompScrew2
877     Mov PInitialPosition
878     fErrorProcess(11,54,52,0)
879     If M_20# = MNext% Then M_20# = MClear%
880     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
881     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
882     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
883     *CompScrew2
884     '
885     'Soc��p�l�W�����@�փl�W�����ɍs��
886     *RE_SCREW_GET_3
887     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
888     If MRtn = 1 Then GoTo *CompScrewGet_3
889     If M_20# = MNext% Then M_20# = MClear%
890     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
891     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
892     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
893     *CompScrewGet_3
894     '
895     PScrewPos(1) = PScrewSoc3_1             ' �˂��s�b�N�A�b�v������
896     PScrewPos(2) = PScrewSoc3_0             ' �l�W3���ߊJ�n�ʒu����(10/8 M.H)
897     PScrewPos(10) = PScrewSoc3              ' �˂������@���_����
898     '�B�ԃl�W����
899     M_Out16(12672) = 3              '�l�W���߈ʒu�ԍ����M
900     MRtn = ScrewTight(PScrewPos,1,10.0)
901     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
902     If MRtn = 1 Then GoTo *CompScrew3
903     Mov PInitialPosition
904     fErrorProcess(11,55,52,0)
905     If M_20# = MNext% Then M_20# = MClear%
906     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
907     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
908     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
909     *CompScrew3
910     '
911     'Soc��p�l�W�����@�փl�W�����ɍs��
912     *RE_SCREW_GET_4
913     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
914     If MRtn = 1 Then GoTo *CompScrewGet_4
915     If M_20# = MNext% Then M_20# = MClear%
916     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
917     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
918     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
919     *CompScrewGet_4
920     '
921     PScrewPos(1) = PScrewSoc4_1             ' �˂��s�b�N�A�b�v������
922     PScrewPos(2) = PScrewSoc4_0             ' �l�W4���ߊJ�n�ʒu����(10/8 M.H)
923     PScrewPos(10) = PScrewSoc4              ' �˂������@���_����
924     '�C�ԃl�W����
925     M_Out16(12672) = 4              '�l�W���߈ʒu�ԍ����M
926     MRtn = ScrewTight(PScrewPos,1,10.0)
927     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
928     If MRtn = 1 Then GoTo *CompScrew4
929     Mov PInitialPosition
930     fErrorProcess(11,56,52,0)
931     If M_20# = MNext% Then M_20# = MClear%
932     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
933     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
934     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
935     *CompScrew4
936     '
937     'Soc��p�l�W�����@�փl�W�����ɍs��
938     *RE_SCREW_GET_5
939     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
940     If MRtn = 1 Then GoTo *CompScrewGet_5
941     If M_20# = MNext% Then M_20# = MClear%
942     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
943     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
944     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
945     *CompScrewGet_5
946     '
947     PScrewPos(1) = PScrewSoc5_1             ' �˂��s�b�N�A�b�v������
948     PScrewPos(2) = PScrewSoc5_0             ' �l�W5���ߊJ�n�ʒu����(10/8 M.H)
949     PScrewPos(10) = PScrewSoc5              ' �˂������@���_����
950     '�D�ԃl�W����
951     M_Out16(12672) = 5              '�l�W���߈ʒu�ԍ����M
952     MRtn = ScrewTight(PScrewPos,1,10.0)
953     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
954     If MRtn = 1 Then GoTo *CompScrew5
955     Mov PInitialPosition
956     fErrorProcess(11,57,52,0)
957     If M_20# = MNext% Then M_20# = MClear%
958     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
959     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
960     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
961     *CompScrew5
962     '
963     'Soc��p�l�W�����@�փl�W�����ɍs��
964     *RE_SCREW_GET_6
965     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
966     If MRtn = 1 Then GoTo *CompScrewGet_6
967     If M_20# = MNext% Then M_20# = MClear%
968     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
969     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
970     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
971     *CompScrewGet_6
972     '
973     PScrewPos(1) = PScrewSoc6_1             ' �˂��s�b�N�A�b�v������
974     PScrewPos(2) = PScrewSoc6_0             ' �l�W6���ߊJ�n�ʒu����(10/8 M.H)
975     PScrewPos(10) = PScrewSoc6              ' �˂������@���_����
976     '�E�ԃl�W����
977     M_Out16(12672) = 6              '�l�W���߈ʒu�ԍ����M
978     MRtn = ScrewTight(PScrewPos,1,10.0)
979     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
980     If MRtn = 1 Then GoTo *CompScrew6
981     Mov PInitialPosition
982     fErrorProcess(11,58,52,0)
983     If M_20# = MNext% Then M_20# = MClear%
984     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
985     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
986     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
987     *CompScrew6
988 '
989     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
990     Mov PTicketRead_1   '�`�P�b�g�ǂݎ��ʒu���
991     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
992     InitialState()  ' ������Ԃɂ���*AssyEnd
993     M_20# = MAssyOK%              ' ����I������
994     GoTo *fnAssyStart_FEndPosi
995 '
996 *ASSY_ERROR_END
997     fnInitialZone()   ' �����ʒu�Ɉړ�
998 *AssyEnd
999     InitialState()  ' ������Ԃɂ���
1000 *fnAssyStart_FEndPosi
1001     Exit Function
1002 FEnd
1003 '
1004 '��fnPiasCheck
1005 ''' <summary>
1006 ''' PIAS�`�P�b�g�Ǎ���
1007 ''' </summary>
1008 ''' <returns>   0 : NG
1009 '''             1 : OK(�Ǎ��݊���)
1010 ''' </returns>
1011 ''' <remarks>
1012 ''' Date   : 2021/07/07 : M.Hayakawa
1013 ''' </remarks>'
1014 Function M% fnPiasCheck
1015     fnPiasCheck = 0
1016     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1017     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1018 '
1019 *RETRY_PIAS
1020     M_20# = MClear%
1021     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1022     '
1023     '�yID�`�P�b�g�ǂݍ��݁z
1024     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1025     MInspGroup%(1) = 1              '����G�ԍ�
1026     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1027 '
1028     '�G���[�̏ꍇ
1029     If MRtn <> 1 Then
1030         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1031         If MRtn <> 1 Then
1032             'D720 -> D1300 �R�s�[�v��
1033             M_Out(12565) = 1
1034             Dly 0.5
1035             M_Out(12565) = 0
1036             '�G���[�����L�q
1037             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1038             'GOT KEY���͑҂�
1039             MKeyNumber = fnKEY_WAIT()
1040             '
1041             Select MKeyNumber
1042                 Case MNext%         '���ւ�I�������ꍇ
1043                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1044                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1045                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1046                     Break
1047                 Case MAbout%        '��~��I�������ꍇ
1048                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1049                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1050                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1051                     Break
1052                 Case MNgProcess%    'NG��I�������ꍇ
1053                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1054                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1055                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1056                     Break
1057                 Case MContinue%     '�p����I�������ꍇ
1058                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1059                     M_20# = MContinue%
1060                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1061                     Break
1062             End Select
1063         EndIf
1064     EndIf
1065 '----------D720 -> D1300 �R�s�[�v��----------
1066     M_Out(12565) = 1
1067     Dly 0.5
1068     M_Out(12565) = 0
1069 '----------�ʐM�m�F������----------
1070     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1071     MRtn = 0                ' ������
1072     M_20# = MClear%         ' ������
1073     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1074     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1075     If MRtn <> 1 Then
1076         If M_20# = MContinue% Then
1077             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1078         Else
1079             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1080         EndIf
1081     EndIf
1082 '----------�H�������m�F----------
1083     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1084     MRtn = 0                ' ������
1085     M_20# = MClear%         ' ������
1086     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1087     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1088     If MRtn <> 1 Then
1089         If M_20# = MContinue% Then
1090             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1091         Else
1092             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1093         EndIf
1094     EndIf
1095     '
1096     fnPiasCheck = 1
1097     *fnPiasCheck_End
1098     Exit Function
1099 FEnd
1100 '
1101 '��fnPCComuCheck
1102 ''' <summary>
1103 ''' PC-PLC�ʐM�`�F�b�N
1104 ''' </summary>
1105 ''' <returns>   0 : NG
1106 '''             1 : OK(�Ǎ��݊���)
1107 ''' </returns>
1108 ''' <remarks>
1109 ''' Date   : 2021/07/07 : M.Hayakawa
1110 ''' </remarks>'
1111 Function M% fnPCComuCheck
1112     fnPCComuCheck = 0
1113     MJudge% = 0                                  '������
1114     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1115     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1116     '
1117     For MStaNo = 0 To 5
1118         '
1119         If M_In(MIN_PIAS_ComOK%) = 1 Then
1120             'PC�ʐMOK(M400)
1121             MJudge% = MOK%
1122             MStaNo = 5
1123             Break
1124         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1125             'toRBT_�ʐM�m�Ftime out
1126             MJudge% = MNG%
1127             MCommentD1001 = 15
1128             MCommentD1002 = 21
1129             MStaNo = 5
1130             Break
1131         Else
1132             'toRBT_�ʐM�m�Ftime out
1133             MJudge% = MNG%
1134             MCommentD1001 = 14
1135             MCommentD1002 = 21
1136             Break
1137         EndIf
1138     Next MStaNo
1139     '
1140     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1141     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1142     '
1143     '�G���[���
1144     If MJudge% <> MOK% Then
1145         M_20# = MClear%     '������
1146         '�G���[�����L�q
1147         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1148         'GOT KEY���͑҂�
1149         MKeyNumber = fnKEY_WAIT()
1150         '
1151         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1152             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1153             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1154             Break
1155         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1156             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1157             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1158             Break
1159         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1160             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1161             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1162             Break
1163         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1164             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1165             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1166             Break
1167         EndIf
1168     Else
1169         'OK�̏ꍇ
1170         fnPCComuCheck = 1
1171     EndIf
1172     Exit Function
1173 FEnd
1174 '
1175 '��fnProcessCheck
1176 ''' <summary>
1177 ''' �H�������m�F
1178 ''' </summary>
1179 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1180 '''             -1�F�O�H������NG  -2�F���H����������
1181 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1182 '''             -5�F���������G���[
1183 ''' </returns>
1184 ''' <remarks>
1185 ''' Date   : 2021/07/07 : M.Hayakawa
1186 ''' </remarks>'
1187 Function M% fnProcessCheck
1188     fnProcessCheck = 0
1189     MJudge% = MNG%      '��UNG���������Ƃ���
1190 '----------�H�������m�F----------
1191     MCommentD1001 = 0   '�R�����g������
1192     For MStaNo = 0 To 5
1193         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1194         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1195         '
1196         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1197             MJudge% = MOK%
1198             fnAutoScreenComment(85)     ' AUTO���
1199             MStaNo = 5
1200             Break
1201         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1202             MFlgLoop% = 0
1203             MJudge% = MNG%
1204             MCommentD1001 = 27
1205             MCommentD1002 = 22
1206             fnAutoScreenComment(94)     ' AUTO���
1207             fnProcessCheck = -2         ' NG��-2��Ԃ�
1208             MStaNo = 5
1209             Break
1210         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1211            MJudge% = MNG%
1212             MCommentD1001 = 31
1213             MCommentD1002 = 22
1214             fnAutoScreenComment(83)     ' AUTO���
1215             fnProcessCheck = -3         ' NG��-3��Ԃ�
1216             MStaNo = 5
1217             Break
1218         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1219             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1220             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1221             MJudge% = MNG%
1222             MCommentD1001 = 32
1223             MCommentD1002 = 22
1224             fnAutoScreenComment(84)     ' AUTO���
1225             fnProcessCheck = -1         ' NG��-1��Ԃ�
1226             Dly 1.0
1227             '�H�������m�FOFF
1228             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1229             Dly 1.0
1230            'MStaNo = 5
1231             Break
1232         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1233             MFlgLoop% = 0
1234             MJudge% = MNG%
1235             MCommentD1001 = 29
1236             MCommentD1002 = 22
1237             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1238             fnProcessCheck = -5         ' NG��-5��Ԃ�
1239             MStaNo = 5
1240             Break
1241         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1242             MJudge% = MNG%
1243             If MCommentD1001 = 32 Then
1244                 '�������Ȃ�
1245             Else
1246                 MCommentD1001 = 26
1247             EndIf
1248             MCommentD1002 = 22
1249             fnProcessCheck = -4         ' NG��-4��Ԃ�
1250             MStaNo = 5
1251             Break
1252         Else
1253             MJudge% = MNG%
1254             MCommentD1001 = 28
1255             MCommentD1002 = 22
1256         EndIf
1257     Next MStaNo
1258     '�H�������m�FOFF
1259     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1260     '�ʉߗ���NG �H�������̏ꍇ
1261     If MJudge% = MPass% Then
1262         M_20# = MPass%
1263     EndIf
1264     '
1265     '�G���[���
1266     If MJudge% <> MOK% Then
1267         M_20# = MClear%     '������
1268         '�G���[�����L�q
1269         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1270         'GOT KEY���͑҂�
1271         MKeyNumber = fnKEY_WAIT()
1272         '
1273         Select MKeyNumber
1274             Case MAbout%        '��~��I�������ꍇ
1275                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1276                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1277                 Break
1278             Case MNext%         '���ւ�I�������ꍇ
1279                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1280                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1281                 Break
1282             Case MContinue%     '�p����I�������ꍇ
1283                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1284                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1285                 Break
1286             Case MNgProcess%    'NG��I�������ꍇ
1287                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1288                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1289                 Break
1290         End Select
1291     Else
1292         fnProcessCheck = 1  ' OK��1��Ԃ�
1293     EndIf
1294     Exit Function
1295 FEnd
1296 '
1297 '��fnPiasWrite
1298 ''' <summary>
1299 ''' Pias �g�����ʏ����ݗv��
1300 ''' </summary>
1301 '''<param name="MFlg%">
1302 '''                 MOK%(1) = �H��������OK��������
1303 '''                 MNG%(0) = �H��������NG��������
1304 '''</param>
1305 '''<returns></returns>
1306 ''' <remarks>
1307 ''' Date   : 2021/07/07 : M.Hayakawa
1308 ''' </remarks>'
1309 Function M% fnPiasWrite(ByVal MFlg%)
1310       fnPiasWrite = 0
1311 *RETRY_PIASWRITE
1312     '
1313     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1314    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1315     If MFlg% = MOK% Then
1316         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1317     Else
1318         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1319     EndIf
1320     Dly 0.1                  '�O�̂���
1321     '
1322     'Pias�֏����݊J�n M305 -> ON
1323     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1324     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1325     '
1326     MJudge% = MNG%
1327     '
1328     For MStaNo = 0 To 5
1329         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1330             MJudge% = MOK%
1331             'MRet = fnAutoScreenComment(85)  'AUTO���
1332             MStaNo = 5
1333             Break
1334         '
1335         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1336             MJudge% = MNG%
1337             'MRet = fnAutoScreenComment(85)  'AUTO���
1338            MCommentD1001 = 34
1339            MCommentD1002 = 25
1340             MStaNo = 5
1341             Break
1342         '
1343         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1344             MJudge% = MNG%
1345             'MRet = fnAutoScreenComment(85)  'AUTO���
1346            MCommentD1001 = 35
1347            MCommentD1002 = 25
1348             MStaNo = 5
1349             Break
1350         '
1351         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1352             MJudge% = MNG%
1353             'MRet = fnAutoScreenComment(85)  'AUTO���
1354            MCommentD1001 = 36
1355            MCommentD1002 = 25
1356             MStaNo = 5
1357             Break
1358         '
1359         Else
1360             MJudge% = MNG%
1361            MCommentD1001 = 42
1362            MCommentD1002 = 25
1363         '
1364         EndIf
1365         '
1366     Next MStaNo
1367     '
1368     'Pias�֏����݊J�n M305 -> OfF
1369     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1370     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1371     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1372     '
1373     '
1374     '�ʉߗ���NG �H�������̏ꍇ
1375     If MJudge% = MPass% Then
1376         M_20# = MPass%
1377     EndIf
1378     '
1379    M_20# = MClear%     '������
1380     '
1381     '�G���[���
1382     If MJudge% < MOK% Then
1383     '
1384 '�c���Ă���������ł͎g�p���Ȃ����x��
1385 *RETRY_ERR_WRITE
1386         M_20# = MClear%     '������
1387         '�G���[�����L�q
1388         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1389         'GOT KEY���͑҂�
1390         MKeyNumber = fnKEY_WAIT()
1391         '
1392         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1393             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1394            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1395             Break
1396         '
1397         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1398             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1399             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1400         '
1401         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1402             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1403             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1404         '
1405         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1406             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1407            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1408             Break
1409         '
1410         EndIf
1411         '
1412         If M_20# = MClear% Then *RETRY_ERR_WRITE
1413         '
1414     EndIf
1415     '
1416     If M_20# = MContinue% Then *RETRY_PIASWRITE
1417     '
1418     fnPiasWrite = 1
1419     Exit Function
1420 FEnd
1421 '
1422 '��fnPCBNumberCheck
1423 ''' <summary>
1424 ''' Pias ��ԍ��ƍ��v��
1425 ''' </summary>
1426 '''<param name="%"></param>
1427 '''<param name="%"></param>
1428 '''<returns></returns>
1429 ''' <remarks>
1430 ''' Date   : 2021/07/07 : M.Hayakawa
1431 ''' </remarks>'
1432 Function M% fnPCBNumberCheck
1433       fnPCBNumberCheck = 0
1434     '
1435 *RETRY_PCBCHECK
1436     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1437     'Pias�֊�ƍ��J�n M310 -> ON
1438     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1439     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1440     '
1441     MJudge% = MNG%
1442     '
1443     For MStaNo = 0 To 5
1444         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1445             MJudge% = MOK%
1446             fnAutoScreenComment(96)  'AUTO���
1447             MStaNo = 5
1448             Break
1449         '
1450         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1451             MJudge% = MNG%
1452             fnAutoScreenComment(97)  'AUTO���
1453             MCommentD1001 = 37
1454             MCommentD1002 = 25
1455             MStaNo = 5
1456             Break
1457         '
1458         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1459             MJudge% = MNG%
1460             fnAutoScreenComment(98)  'AUTO���
1461             MCommentD1001 = 38
1462             MCommentD1002 = 25
1463             MStaNo = 5
1464             Break
1465         '
1466         ElseIf M_In(11580) = 1 Then                         'time out
1467             MJudge% = MNG%
1468             fnAutoScreenComment(99)  'AUTO���
1469             MCommentD1001 = 39
1470             MCommentD1002 = 25
1471             MStaNo = 5
1472             Break
1473         '
1474         Else
1475             MJudge% = MNG%
1476            MCommentD1001 = 41
1477            MCommentD1002 = 25
1478         '
1479         EndIf
1480         '
1481     Next MStaNo
1482     '
1483     'Pias�֊�ƍ��J�n M310 -> OfF
1484     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1485     '
1486     '
1487     '�ʉߗ���NG �H�������̏ꍇ
1488     If MJudge% = MPass% Then
1489         M_20# = MPass%
1490     EndIf
1491     '
1492    M_20# = MClear%     '������
1493     '
1494     '�G���[���
1495     If MJudge% < MOK% Then
1496     '
1497 '�c���Ă���������ł͎g�p���Ȃ����x��
1498 *RETRY_ERR_PCBNUMBER
1499         M_20# = MClear%     '������
1500         '�G���[�����L�q
1501         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1502         'GOT KEY���͑҂�
1503         MKeyNumber = fnKEY_WAIT()
1504         '
1505         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1506             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1507             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1508             Break
1509         '
1510         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1511             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1512             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1513         '
1514         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1515             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1516             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1517         '
1518         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1519             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1520             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1521             Break
1522         '
1523         EndIf
1524         '
1525         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1526         '
1527     EndIf
1528     '
1529     If M_20# = MContinue% Then *RETRY_PCBCHECK
1530     Exit Function
1531 FEnd
1532 '
1533 '��ScrewTight
1534 ''' <summary>
1535 ''' �˂����߂��s��(S�^�C�g)
1536 ''' </summary>
1537 '''<param name="PScrewPos()">
1538 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1539 '''             PScrewPos(2)    �F�˂����߉��_
1540 '''             PScrewPos(10)   �F�˂����ߏI������
1541 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
1542 '''             1:6mm S�^�C�g��l�W
1543 '''             2:8mm P�^�C�g
1544 '''             3:6mm S�^�C�g���l�W
1545 '''             4:13mm S�^�C�g
1546 '''             5:6mm M�l�W
1547 '''</param>
1548 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
1549 '''<returns>����
1550 '''         0=�ُ�I���A1=����I��
1551 '''</returns>
1552 ''' <remarks>
1553 ''' Date   : 2021/07/07 : M.Hayakawa
1554 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
1555 ''' </remarks>'
1556 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
1557     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1558     ScrewTight = 0
1559     MOKNGFlg = 0
1560     Ovrd 100
1561     Mvs PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
1562     Select MScrewType%      '�ǂݍ��݈ʒu�ύX(1/19����)
1563         Case 1
1564             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1565             ProgramBankSet(1,1)
1566             Break
1567         Case 2
1568             ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1569             ProgramBankSet(3,1)
1570             Break
1571         Case 3
1572             ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1573             ProgramBankSet(1,1)
1574             Break
1575         Case 4
1576             ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1577             ProgramBankSet(1,1)
1578             Break
1579         Case 5
1580             ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1581             ProgramBankSet(1,1)
1582             Break
1583         Default
1584             ' �v���O����1�A�o���N�Ȃ��ݒ�
1585             ProgramBankSet(0,0)
1586             Break
1587     End Select
1588     Fine 0.05 , P
1589     Ovrd MOvrdA%
1590     ' ������10�ɐݒ�
1591     Accel 100,10
1592     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
1593     Mvs PScrewPosition(2)
1594     ' �����������ɖ߂�
1595     Accel
1596     ' ����Ovrd�ݒ�
1597 '    Ovrd MOvrdA%
1598     Ovrd 100
1599     ' Spd�ݒ�
1600 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1601     Spd MFeedSpd
1602     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
1603     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1604 '    Select MScrewType%      '�ǂݍ��݈ʒu�ύX(1/19����)
1605 '        Case 1
1606 '            ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1607 '            ProgramBankSet(1,1)
1608 '            Break
1609 '        Case 2
1610 '            ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1611 '            ProgramBankSet(3,1)
1612 '            Break
1613 '        Case 3
1614 '            ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1615 '            ProgramBankSet(1,1)
1616 '            Break
1617 '        Case 4
1618 '            ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1619 '            ProgramBankSet(1,1)
1620 '            Break
1621 '        Case 5
1622 '            ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1623 '            ProgramBankSet(1,1)
1624 '            Break
1625 '        Default
1626 '            ' �v���O����1�A�o���N�Ȃ��ݒ�
1627 '            ProgramBankSet(0,0)
1628 '            Break
1629 '    End Select
1630 '
1631 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1632      '�h���C�o�[ON�@CW
1633     M_Out(12241)=1
1634     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1635     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
1636     Dly 0.1
1637     Fine 0 , P
1638     Spd M_NSpd
1639     '
1640     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
1641         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1642         Dly 0.1
1643        ' �v���O�����E�o���N����
1644         ProgramBankSet(0,0)
1645         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1646         Mvs PScrewPosition(10),-80
1647         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1648         M_Out(12249)=1 Dly 0.3
1649         MOKNGFlg = -1
1650         ScrewTight = 0
1651     Else
1652          '�h���C�o�[OFF�@CW
1653         M_Out(12241)=0
1654 ''        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
1655 '        Select MScrewType%
1656 '            Case 1
1657 '                ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1658 '                ProgramBankSet(1,3)
1659 '                Break
1660 '            Case 2
1661 '                ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1662 '                ProgramBankSet(3,3)
1663 '                Break
1664 '            Case 3
1665 '                ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1666 '                ProgramBankSet(1,3)
1667 '                Break
1668 '            Case 4
1669 '                ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1670 '                ProgramBankSet(1,3)
1671 '                Break
1672 '            Case 5
1673 '                ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1674 '                ProgramBankSet(1,3)
1675 '                Break
1676 '            Default
1677 '                ' �v���O����1�A�o���N�Ȃ��ݒ�
1678 '                ProgramBankSet(0,0)
1679 '                Break
1680 '        End Select
1681 '         '�h���C�o�[ON�@CW
1682 '        Mvs PScrewPosition(10)
1683 '        M_Out(12241)=1
1684 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1685 '        Fine 0 , P
1686 ''
1687          '�h���C�o�[OFF�@CW
1688         M_Out(12241)=0
1689        ' �v���O�����E�o���N����
1690         ProgramBankSet(0,0)
1691         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1692         M_Out(12249)=1 Dly 0.3
1693     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1694         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1695         Mvs PScrewPosition(10),-80
1696         ScrewTight = 1
1697     EndIf
1698 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1699 '    Ovrd 10
1700 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1701     Ovrd 100
1702     Exit Function
1703 FEnd
1704 '
1705 '��ScrewGet
1706 ''' <summary>
1707 ''' �˂������@����˂��𓾂�
1708 ''' </summary>
1709 '''<param name="%">
1710 '''         PScrewPos(1)    �F�˂�������̂˂����
1711 '''         PScrewPos(2)    �F�˂���������_
1712 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1713 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1714 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1715 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1716 '''</param>
1717 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1718 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1719 '''<returns>����
1720 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1721 '''</returns>
1722 ''' <remarks>
1723 ''' Date   : 2021/07/07 : M.Hayakawa
1724 ''' </remarks>
1725 '''<update>
1726 '''Date    : 2021/11/15 : ����
1727 '''</update>
1728 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1729     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
1730     ScrewGet = 0
1731     MScrewJudge% = 0
1732     '�˂������평������G���[�`�F�b�N
1733 ' ���b��폜
1734     'Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
1735     For MCnt% = 0 To MFinCnt%
1736         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1737         If MRtn = 0 Then
1738             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1739             ScrewGet = -1
1740             MScrewJudge% = 2
1741         EndIf
1742         Ovrd 100
1743         If FeederScrewSensor% <> 0 Then
1744             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1745                 'Ovrd 30
1746                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1747                 'NG�Ƃ��Ă����̊֐����甲����
1748                 ScrewGet = -2
1749                 MScrewJudge% = 3
1750             EndIf
1751         EndIf
1752         Ovrd 100
1753         Spd M_NSpd
1754         If MScrewJudge% = 0 Then
1755     '        ScrewGet = 0
1756             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1757             Dly 0.3
1758             MScrewCnt% = 0
1759             MFinCnt% = 2
1760             fnAutoScreenComment(521)     '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1761             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1762             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1763             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1764             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1765             'Mvs PScrewPosition(10), 1.2
1766            Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
1767             '�r�b�g��](�����ʒu�ύX)
1768             M_Out(Y60_Driver)=1
1769             M_Timer(4) = 0
1770             MloopFlg = 0
1771             MCntTime& = 0
1772             While MloopFlg = 0
1773                 MCrtTime& = M_Timer(4)
1774                 If MCrtTime& >= 180 Then
1775                     MloopFlg = 1
1776                 EndIf
1777             WEnd
1778             M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1779             '�z���m�F
1780             MRtn = 0
1781             MRtn = frInCheck(11271, 1, MSETTIMEOUT01&)
1782 '            '�r�b�g��](�����ʒu�ύX)
1783 '            M_Out(Y60_Driver)=1
1784 '            Dly 0.2
1785             '
1786             JOvrd M_NJovrd
1787             Spd M_NSpd
1788             '�l�W�z���m�F�ʒu�ړ�
1789             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1790             Mvs PScrewPosition(10), -30  ' �l�W�z���m�F�ʒu
1791            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1792             '�r�b�g��]��~
1793             M_Out(Y60_Driver)=0
1794             '
1795 '            If MRtn = 1 Then           '�ŏ���臒l���m�F���Ȃ�
1796                 '1�b�ԃl�W�z���m�F
1797                 MRtn = frInCheck(11272, 1, MSETTIMEOUT01&)
1798 '            EndIf
1799             'MRtn = 0'�����G���[
1800             '�z���G���[�̏ꍇ
1801             '�l�W���˂����Y�ɖ߂�
1802             If MRtn = 0 Then
1803                 Ovrd 30      '2����5�ɕύX
1804                 '�r�b�g��]��~
1805                 M_Out(Y60_Driver)=0
1806                 '�l�W�����@���
1807                 Mvs PScrewPosition(1)
1808                 '�X�ɏ��
1809                 Mov PScrewPosition(1), -140
1810                 '�l�W�̂Ĉʒu
1811                 MRtn = FnCtlValue2(4)          '�z���G���[���{�P  2022/04/28 �n��
1812                 Mov PScrewPosition(9)
1813                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1814                 '�z��OFF
1815                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1816                 Dly 0.2
1817                 '�j��ON
1818                 M_Out(Y6B_VB1)=1 '�^��j��ON
1819                 '�r�b�g��]
1820                 M_Out(Y61_Driver)=1
1821                 Dly 0.5
1822                 '                '
1823                 Ovrd 100
1824                 JOvrd M_NJovrd
1825                 Spd M_NSpd
1826                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1827                 Mov PScrewPosition(9), 10
1828                 Mov PScrewPosition(9)
1829                 Dly 0.1
1830                 Mov PScrewPosition(9), 10
1831                 Mov PScrewPosition(9)
1832                 '
1833                 '�l�W�����҂�
1834                 Wait M_In(11272) = 0
1835                 '�r�b�g��]��~
1836                 M_Out(Y61_Driver)=0
1837                 Dly 0.1
1838                 '�j��OFF
1839                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1840                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1841                 Mov PScrewPosition(1), -140
1842                 Ovrd 100
1843                 Spd M_NSpd
1844                 '�l�W�����@���
1845                 Mvs PScrewPosition(1)
1846 '                '
1847                 ScrewGet = -3
1848                 If MCnt% = MFinCnt% Then
1849                     MScrewJudge% = 4
1850                     Mov PScrewPosition(2)
1851                     Break
1852                 EndIf
1853                 Break
1854 '                '
1855             Else
1856                 MCnt% = MFinCnt%
1857                 ScrewGet = 1
1858             EndIf
1859         Else
1860             MCnt% =MFinCnt%
1861         EndIf
1862     Next  MCnt%
1863         '
1864 '    If MScrewJudge% = 0 Then
1865 '        Ovrd 100
1866 '        Spd M_NSpd
1867 '        PScrewPosition(1)
1868 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1869 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1870 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1871 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1872 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1873 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1874 '        'Mov PScrewPosition(2)
1875 '        '������x�z���m�F�@���̍ŏI臒l
1876 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1877 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1878 '            MScrewJudge% = 4
1879 '            ScrewGet = -3
1880 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1881 '            MScrewJudge% = 1
1882 '            ScrewGet = 1
1883 '        EndIf
1884 '        Break
1885 '    EndIf
1886     '
1887 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1888     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1889     '
1890     Select MScrewJudge%
1891 '        Case 0
1892 ''            fErrorProcess(11,162,163,0) '�ُ�I��
1893 '            MCommentD1001 = 162
1894 '            MCommentD1002 = 96
1895 '            Break
1896         Case 2
1897 '            fErrorProcess(11,63,161,0) '����NG
1898             MCommentD1001 = 63
1899             MCommentD1002 = 96
1900             Break
1901         Case 3
1902 '            fErrorProcess(11,160,164,0) '�닟��
1903             MCommentD1001 = 237
1904             MCommentD1002 = 96
1905             Break
1906         Case 4
1907 '            fErrorProcess(11,94,95,0) '�z��NG
1908             MCommentD1001 = 94
1909             MCommentD1002 = 95
1910             Break
1911     End Select
1912     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1913     '
1914     Select M_20#
1915         Case MAbout%          '��~�������ꂽ�ꍇ
1916             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
1917             Mov PInitialPosition
1918             Break
1919         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1920             Break
1921         Case MNext%           '�p���������ꂽ�ꍇ
1922             M_20# = MClear%     '������
1923             Break
1924         Case MNgProcess%      'NG�������ꂽ�ꍇ
1925             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
1926             Mov PInitialPosition
1927             Break
1928         End Select
1929 *End_ScrewGet
1930     Exit Function
1931 FEnd
1932 '
1933 '��ProgramBankSet
1934 ''' <summary>
1935 ''' �˂����߂��s��(P�^�C�g)
1936 ''' </summary>
1937 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1938 '''<param name="MBankNo">�o���N�ԍ�</param>
1939 '''</returns>
1940 ''' <remarks>
1941 ''' Date   : 2021/10/05 : M.Hayakawa
1942 ''' </remarks>'
1943 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1944 '
1945     MLocalPrgNo% = (MProgramNo% - 1) * 32
1946     MLocalBankNo% = MBankNo% * 4
1947 '
1948     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1949         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1950     Else
1951         MLocalOutNo% = 0
1952     EndIf
1953 '
1954     M_Out8(12240) = MLocalOutNo%
1955     Dly 0.1
1956     Exit Function
1957 FEnd
1958 '
1959 '��fnKEY_WAIT()
1960 ''' <summary>
1961 ''' GOT����̃L�[���͑҂�
1962 ''' </summary>
1963 '''<returns>1�F��~    2�F����
1964 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1965 '''         5�FNG
1966 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1967 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1968 '''</returns>
1969 ''' <remarks>
1970 ''' Date   : 2021/07/07 : M.Hayakawa
1971 ''' </remarks>'
1972 Function M% fnKEY_WAIT()
1973     fnKEY_WAIT = 0
1974     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1975     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1976     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1977     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1978     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1979     Dly 0.2
1980     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1981     MLocalLoopFlg=1
1982     While MLocalLoopFlg=1
1983         If M_In(11345) = 1 Then         '��~   M5345
1984             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1985             fnKEY_WAIT = 1
1986             MLocalLoopFlg=-1
1987             Break
1988         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1989             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1990             fnKEY_WAIT = 2
1991             MLocalLoopFlg=-1
1992             Break
1993         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1994             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1995             fnKEY_WAIT = 3
1996             MLocalLoopFlg=-1
1997             Break
1998         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1999             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2000             fnKEY_WAIT = 4
2001             MLocalLoopFlg=-1
2002             Break
2003         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2004             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2005             fnKEY_WAIT = 5
2006             MLocalLoopFlg=-1
2007             Break
2008             '
2009         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2010             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2011             fnKEY_WAIT = MRobotInit1%
2012             MLocalLoopFlg=-1
2013             Break
2014             '
2015         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2016             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2017             fnKEY_WAIT = MRobotInit2%
2018             MLocalLoopFlg=-1
2019             Break
2020             '
2021         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2022             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2023             fnKEY_WAIT = MRobotInit3%
2024             MLocalLoopFlg=-1
2025             Break
2026             '
2027         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2028             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2029             fnKEY_WAIT = MRobotInit4%
2030             MLocalLoopFlg=-1
2031             Break
2032             '
2033         Else
2034         EndIf
2035     WEnd
2036     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2037     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2038     Exit Function
2039 FEnd
2040 '
2041 '�� fnAUTO_CTL
2042 ''' <summary>
2043 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2044 ''' </summary>
2045 ''' <remarks>
2046 ''' Date   : 2021/07/07 : M.Hayakawa
2047 ''' </remarks>
2048 Function M% fnAUTO_CTL
2049     fnAUTO_CTL = 0
2050     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2051     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2052     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2053     '
2054     If M_Svo=0 Then             '�T�[�{ON�m�F
2055         Servo On
2056     EndIf
2057     Wait M_Svo=1
2058     Exit Function
2059 FEnd
2060 '
2061 '�� fnWindScreenOpen
2062 ''' <summary>
2063 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2064 ''' </summary>
2065 '''<param name="%"></param>
2066 '''<param name="%"></param>
2067 '''<param name="%"></param>
2068 '''<param name="%"></param>
2069 ''' <remarks>
2070 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2071 ''' MWindReSet = 0     ��ʔ�\��
2072 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2073 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2074 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2075 ''' Date   : 2021/07/07 : M.Hayakawa
2076 ''' </remarks>
2077 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2078     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2079         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2080     EndIf
2081     '
2082     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2083         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2084     EndIf
2085     '
2086     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2087        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2088     EndIf
2089     '
2090     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2091     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2092     Dly 0.5
2093     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2094     Exit Function
2095 FEnd
2096 '
2097 '��FnCtlValue2
2098 ''' <summary>
2099 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2100 ''' </summary>
2101 ''' <param name="MCtlNo%"></param>
2102 ''' <remarks>
2103 ''' Date : 2022/04/28 �n��
2104 ''' </remarks>
2105 '''
2106 '''  1�F������       �{�P
2107 '''  2�F�g���n�j��   �{�P
2108 '''  3�F�g���m�f��   �{�P (���g�p)
2109 '''  4�F�z���G���[�� �{�P
2110 ''' 99�F�Ǐ��J�n�M�� OFF
2111 '''
2112 Function M% FnCtlValue2(ByVal MCtlNo%)
2113     FnCtlValue2 = 1
2114     Select MCtlNo%
2115         Case 1        '�������{�P
2116             M_Out(12569) = 0             '�����݊J�n�M��OFF
2117             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2118             MInputQty = M_In16(11600)    '��������M
2119             MInputQty = MInputQty + 1    '�������{�P
2120             M_Out16(12592) = MInputQty   '���������M
2121             M_Out(12569) = 1             '�����݊J�n�M��ON
2122             Break
2123             '
2124         Case 2        '�g���n�j���{�P
2125             M_Out(12569) = 0             '�����݊J�n�M��OFF
2126             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2127             MAssyOkQty = M_In16(11616)   '�g��OK����M
2128             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2129             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2130             M_Out(12569) = 1             '�����݊J�n�M��ON
2131             Break
2132             '
2133         Case 4        '�z���G���[���{�P
2134             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2135             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2136             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2137             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2138             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2139             M_Out(12569) = 1                       '�����݊J�n�M��ON
2140             Break
2141             '
2142         Case 99        '�Ǐ��J�n�M��OFF
2143             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2144             M_Out(12569) = 0        '�����݊J�n�M��OFF
2145             Break
2146             '
2147     End Select
2148     Exit Function
2149 FEnd
2150 '
2151 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2152 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2153 '-------------------------------------------------------------------------------
2154 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2155 '   ����
2156 '       PInspPos()      �F�����ʒu
2157 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2158 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2159 '       MInspCnt%       �F�����ʒu��
2160 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2161 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2162 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2163 '   �߂�l�F����
2164 '       0=�ُ�I���A1=����I��
2165 '
2166 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2167 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2168 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2169 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2170 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2171 '-------------------------------------------------------------------------------
2172     '----- �����ݒ� -----
2173     Cnt 0                                                           '�ړ�����������(�����l=0)
2174     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2175 '    Cnt 1,0.1,0.1
2176     '�ϐ��錾�E������
2177     Def Inte MNum                                                   '�����ԍ�(������1�`)
2178     MNum% = 1                                                       '�����ԍ������l�ݒ�
2179     Def Inte MEndFlg                                                '�����I���t���O
2180     MEndFlg% = 0
2181     '
2182     '����G�ԍ��ݒ�v���E�������s�v��off
2183     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2184     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2185     '�G���[�ԍ��N���A
2186     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2187     M_Out16(MOUT_InspErrNum) = MInspErrNum
2188     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2189     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2190     '
2191     'Insight Ready check?
2192     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2193         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2194         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2195         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2196         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2197         Exit Function
2198     EndIf
2199     '
2200     '�����ʒu���m�F
2201     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2202         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2203         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2204         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2205         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2206         Exit Function
2207     EndIf
2208     '
2209     '
2210     '
2211     '----- ���C������ -----
2212     '�ݒ肳�ꂽ�����ʒu�����̌������s
2213     While( MEndFlg% = 0 )
2214         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2215         MSetGrNumRetryExitFlg = 0
2216         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2217         While( MSetGrNumRetryExitFlg = 0 )
2218         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2219             '
2220             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2221             '
2222             '----- �����O���[�v�ԍ��ݒ� -----
2223             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2224             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2225             '
2226             '�����ʒu�ֈړ��E�ړ������҂�
2227             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2228             Mvs PInspPos( MNum% )                                       '�ړ�
2229             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2230             Dly 0.05                                                    '�ړ�������Delay
2231             '
2232             '�����O���[�v�ԍ��ݒ�I���m�F
2233             M_Timer(1) = 0
2234             MExitFlg = 0
2235             While( MExitFlg = 0 )
2236                 '����G�ݒ萳��I��?
2237                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2238                     MExitFlg = 1
2239                 '
2240                 '����G�ݒ�ُ�I��?
2241                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2242                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2243                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2244                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2245                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2246                     EndIf
2247                     MExitFlg = 1
2248                 '
2249                 'timeout�`�F�b�N
2250                 ElseIf 1000 < M_Timer(1) Then
2251                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2252                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2253                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2254                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2255                     EndIf
2256                     MExitFlg = 1
2257                 EndIf
2258             WEnd
2259             '
2260             '����G�ԍ��ݒ�v��off
2261             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2262             '
2263             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2264             'NG�Ȃ���Δ�����
2265             If MCurrentStepErr = 0 Then
2266                 MSetGrNumRetryExitFlg = 1
2267             Else
2268                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2269                 If MSetGrNumRetryCnt = 0 Then
2270                     MSetGrNumRetryExitFlg = 1
2271                 Else
2272                     'Retry�ց@���̑O��Delay
2273                     Dly 0.5
2274                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2275                 EndIf
2276             EndIf
2277             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2278             '
2279         WEnd
2280         '
2281         '
2282         '
2283         '----- �������s -----
2284         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2285             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2286                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2287                 MInspRetryExitFlg = 0
2288                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2289                 While( MInspRetryExitFlg = 0 )
2290                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2291                     '
2292                     '���������m�F
2293                     MRetryCnt = MRetryCnt - 1
2294                     M_Timer(1) = 0
2295                     MExitFlg = 0
2296                     While( MExitFlg = 0 )
2297                     '���������҂�
2298                         '����OK�I��?
2299                         If M_In( MIN_IS_InspOK% ) = 1  Then
2300                             MJudgeOKFlg = 1                         '����OK�t���OON
2301                             MExitFlg = 1
2302                         '
2303                         '����NG�I��?
2304                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2305                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2306                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2307                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2308                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2309                                 EndIf
2310                             EndIf
2311                             MExitFlg = 1
2312                         '
2313                         '�����ُ�I��(IS timeout)?
2314                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2315                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2316                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2317                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2318                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2319                                 EndIf
2320                             EndIf
2321                             MExitFlg = 1
2322                         '
2323                         'timeout�`�F�b�N
2324                         ElseIf 3000 < M_Timer(1) Then
2325                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2326                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2327                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2328                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2329                                 EndIf
2330                             EndIf
2331                             MExitFlg = 1
2332                         EndIf
2333                     WEnd
2334                     '
2335                     '�����J�n�v��off
2336                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2337                     '
2338                     'OK�Ȃ甲����
2339                     If MJudgeOKFlg = 1 Then
2340                         MInspRetryExitFlg = 1
2341                     Else
2342                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2343                         If MRetryCnt = 0 Then
2344                             MInspRetryExitFlg = 1
2345                         Else
2346                             'Retry�ց@���̑O��Delay
2347                             Dly 0.3
2348                         EndIf
2349                     EndIf
2350                     '
2351                 WEnd
2352             EndIf
2353         EndIf
2354         '
2355         '
2356         '
2357         MNum% = MNum% + 1                                           '����Step+1
2358         '�����I���m�F�@�����I���t���O�Z�b�g
2359         If (MInspCnt% < MNum% ) Then
2360             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2361         EndIf
2362         'NG���������s������
2363         If MInspErrNum <> 0 Then                                    'NG����?
2364             If MNgContinue% <> 1 Then                               'NG���s?
2365                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2366             EndIf
2367         EndIf
2368     WEnd
2369     '
2370     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2371     If 0 < MZAxis% Then
2372         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2373         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2374         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2375         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2376     EndIf
2377     '
2378     '�߂�l�ݒ�
2379     If MInspErrNum = 0 Then
2380         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2381     Else
2382         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2383         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2384         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2385     EndIf
2386     Exit Function
2387 FEnd
2388 '
2389 '��fnAutoScreenComment
2390 ''' <summary>
2391 ''' ���C����ʂ̓���󋵕\��
2392 ''' �R�����gD1005�̐ݒ�
2393 ''' </summary>
2394 '''<param name="McommentD1005%">�R�����gID</param>
2395 ''' <remarks>
2396 ''' Date   : 2021/07/07 : M.Hayakawa
2397 ''' </remarks>
2398 Function fnAutoScreenComment(ByVal McommentD1005%)
2399     M_Out16(12576) = McommentD1005%
2400     Exit Function
2401 FEnd
2402 '
2403 '��fnRoboPosChk
2404 ''' <summary>
2405 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2406 ''' </summary>
2407 '''<param name="MINNumber%">���͔ԍ�</param>
2408 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2409 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2410 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2411 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2412 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2413 ''' <remarks>
2414 ''' Date   : 2021/07/07 : M.Hayakawa
2415 ''' </remarks>
2416 Function M% fnRoboPosChk
2417     fnRoboPosChk = 0
2418     MRet = fnStepRead()
2419     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2420     '�E�B���h��ʐ؊���
2421     If MRBTOpeGroupNo > 5 Then
2422         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2423         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2424         Dly 0.2
2425         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2426         Dly 1.5
2427         '
2428         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2429         '
2430         MLoopFlg% = 1
2431         While MLoopFlg% = 1
2432             '
2433             '
2434             MKeyNumber% = fnKEY_WAIT()
2435             Select MKeyNumber%
2436                 Case Is = MAbout%       '��~
2437                     M_20# = MAbout%
2438                     MLoopFlg% = -1
2439                     Break
2440                 Case Is = MNext%        '����
2441                     'MLoopFlg% = -1
2442                     Break
2443                 Case Is = MContinue%    '�p��
2444                     M_20# = MContinue%
2445                     MLoopFlg% = -1
2446                     Break
2447                 Default
2448                     Break
2449             End Select
2450         WEnd
2451     EndIf
2452     '
2453     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2454         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2455         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2456         Select MRBTOpeGroupNo
2457             Case Is = 5                          '�������Ȃ�
2458                 Break
2459             Case Is = 10                         '�����ʒu�֖߂�
2460                 'Mov PTEST001
2461                 Break
2462             Case Is = 15                         '�����ʒu�֖߂�
2463                 'Mov PTEST002
2464                 Dly 0.5
2465                 'Mov PTEST001
2466                 Dly 0.5
2467                 Break
2468             Default
2469                 Break
2470         End Select
2471         '
2472         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2473         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2474         MRBTOpeGroupNo = 5
2475         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2476         Dly 1.0
2477         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2478         fnRoboPosChk = 1                        '�����ʒu������s
2479         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2480     EndIf
2481     Exit Function
2482 FEnd
2483 '
2484 '��frInCheck
2485 ''' <summary>
2486 ''' �Z���T�[IN�`�F�b�N
2487 ''' </summary>
2488 '''<param name="MINNumber%">���͔ԍ�</param>
2489 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2490 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2491 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2492 ''' <remarks>
2493 ''' Date   : 2021/07/07 : M.Hayakawa
2494 ''' </remarks>
2495 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2496     M_Timer(4) = 0
2497     MloopFlg = 0
2498     While MloopFlg = 0
2499         MCrtTime& = M_Timer(4)
2500         If M_In(MINNumber%) = MCMPFLG% Then
2501             MloopFlg = 1
2502             frInCheck = 1
2503         ElseIf MCrtTime& > MTimeCnt& Then
2504             MloopFlg = 1
2505             frInCheck = 0
2506         EndIf
2507     WEnd
2508     Exit Function
2509 FEnd
2510 '-----------------------------------------------
2511 '
2512 '�˂����ߋ@�ʐM�m�F
2513 '
2514 '-----------------------------------------------
2515 Function M% fScewTcomChk
2516     fScewTcomChk = 0
2517     '�ʐM�m�F���M
2518     M_Out(MOUT_ScwT_ComChk%) = MOn%
2519     '�ʐM�m�F��M�ҋ@
2520     Wait M_In(MIN_ScwT_comOK%) = MOn%
2521     '�ʐM�m�F���M�I��
2522     M_Out(MOUT_ScwT_ComChk%) = MOff%
2523     Exit Function
2524 FEnd
2525 '
2526 '
2527 '-----------------------------------------------
2528 '
2529 '�˂����ߊJ�n���M
2530 '
2531 '-----------------------------------------------
2532 Function M% fScewTStart
2533     fScewTStart = 0
2534     '�˂����ߊJ�n�ҋ@����M
2535     Wait M_In(MIN_ScwT_STRec%) = MOn%
2536     Dly 0.1
2537     '�˂����ߊJ�n��M�𑗐M
2538     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
2539     Exit Function
2540 FEnd
2541 '
2542 '
2543 '-----------------------------------------------
2544 '
2545 '�˂����ߊ�����M
2546 '
2547 '-----------------------------------------------
2548 Function M% fScewTFinish
2549     fScewTFinish = 0
2550     '�˂����ߊ����ҋ@����M
2551     Wait M_In(MIN_ScwT_Fin%) = MOn%
2552     Dly 0.1
2553     '�˂����ߊ�����M�𑗐M
2554     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
2555     Exit Function
2556 FEnd
2557 '
2558 '
2559 '-----------------------------------------------
2560 '
2561 '����xx��~��M
2562 '
2563 '-----------------------------------------------
2564 Function M% fScewTCaseStop(ByVal MCase%())
2565     fScewTCaseStop = 0
2566     '����xx��~����M
2567     Wait M_In(MCase%(1)) = MOn%
2568     Dly 0.1
2569     '����xx��~��M�𑗐M
2570     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
2571     Exit Function
2572 FEnd
2573 '
2574 '-----------------------------------------------
2575 '
2576 '�ĊJ�n��M
2577 '
2578 '-----------------------------------------------
2579 Function M% fScewTReStart()
2580     fScewTReStart = 0
2581     '�ĊJ�n����M
2582     Wait M_In(MIN_ScwT_ReST%) = MOn%
2583     Dly 0.1
2584     '�ĊJ�n��M�𑗐M
2585     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
2586     Exit Function
2587 FEnd
2588 '
2589 '��fErrorProcess
2590 '<summary>
2591 '�G���[����
2592 '</summary>
2593 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2594 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2595 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2596 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2597 '<make>
2598 '2021/11/5 �����V��
2599 '</make>
2600 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2601     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2602     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2603     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2604     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2605 *RETRY_ERR_PROCESS
2606      M_20# = MClear%     '������
2607 '        '�G���[�����L�q
2608         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2609 '        'GOT KEY���͑҂�
2610         MKeyNumber = fnKEY_WAIT()
2611 '        '
2612         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2613             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2614 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2615             Break
2616          '
2617         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2618             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2619 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2620         '
2621         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2622             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2623 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2624          '
2625         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2626             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2627 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2628             Break
2629         '
2630         EndIf
2631         '
2632         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2633         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2634     Exit Function
2635 FEnd
2636 '
2637 '��fnInitialZone
2638 ''' <summary>
2639 ''' ���݈ʒu������ɑҔ����A�����ʒu�ɖ߂�
2640 ''' </summary>
2641 ''' <param name="posNum%">�ړ���̃|�W�V�����ԍ�</param>
2642 ''' <remarks>
2643 ''' Date : 2021/12/2 : M.Hayakawa
2644 ''' Update:2022/06/2 : M.Hayakawa ���H���̔���~���A�ɍ��킹�ĕύX
2645 ''' </remarks>
2646 Function fnInitialZone()
2647     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���]
2648 '
2649     Ovrd 5
2650 ' ���ޔ�
2651     PActive = P_Curr
2652     Pmove = PActive
2653 '
2654     If PActive.X > 580 Then
2655         Pmove.Z =380        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2656     Else
2657         Pmove.Z =500        '��L�ȊO��Z:500�܂Ŏ����グ
2658     EndIf
2659 '
2660     Mvs Pmove
2661     Mov PInitialPosition
2662 ' ���b�N���J��
2663     InitialState()
2664 ' ��U��~
2665     fErrorProcess(20,70,256,0)
2666     Exit Function
2667  FEnd
2668 '
2669 '��InitialState
2670 ''' <summary>
2671 ''' �n���h�A����������ʒu�ɂ���
2672 ''' </summary>
2673 ''' <returns>   0 : OK
2674 '''             1 : NG
2675 ''' </returns>
2676 ''' <remarks>
2677 ''' Date : 2021/12/2 : M.Hayakawa
2678 ''' </remarks>
2679 Function M% InitialState()
2680     InitialState = 0
2681     '
2682     '�ʒu���߉���
2683     M_Out(12264)=0
2684     M_Out(12265)=1 Dly 0.3                  '�v�b�V������
2685     'Wait M_In(11276)=1                      '�v�b�V���ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2686     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    '�v�b�V���ʒu�ߒ[���o(8/26����)
2687     If MRtn = 0 Then
2688         fErrorProcess(11,234,284,0)
2689         Select M_20#
2690             Case MAbout%                    '��~�������ꂽ�ꍇ
2691                 InitialState = 1
2692                 Break
2693             Case MNgProcess%
2694                 InitialState = 1
2695                 Break
2696             Case MContinue%                 '���g���C�������ꂽ�ꍇ
2697                 M_20# = MClear%
2698                 InitialState = 0
2699                 Break
2700             Case MNext%                     '���ւ������ꂽ�ꍇ
2701                 M_20# = MClear%
2702                 InitialState = 0
2703                 Break
2704         End Select
2705     EndIf
2706     *RETRY_POSITIONING_RESTORE
2707     '
2708     M_Out(12262)=0
2709     M_Out(12263)=1 Dly 0.3                  '�ʒu���߉���
2710     'Wait M_In(11274)=1                      '�ʒu���ߖߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2711     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[���o(8/26����)
2712     If MRtn = 0 Then
2713         fErrorProcess(11,234,284,0)
2714         Select M_20#
2715             Case MAbout%                    '��~�������ꂽ�ꍇ
2716                 InitialState = 1
2717                 Break
2718             Case MNgProcess%
2719                 InitialState = 1
2720                 Break
2721             Case MContinue%                 '���g���C�������ꂽ�ꍇ
2722                 M_20# = MClear%
2723                 InitialState = 0
2724                 Break
2725             Case MNext%                     '���ւ������ꂽ�ꍇ
2726                 M_20# = MClear%
2727                 InitialState = 0
2728                 Break
2729         End Select
2730     EndIf
2731     Exit Function
2732 FEnd
2733 '
2734 '��fnTorqueCheck
2735 ''' <summary>
2736 ''' �g���N�`�F�b�N����p�̃��C��
2737 ''' </summary>
2738 ''' <remarks>
2739 ''' Date   : 2021/12/21 : H.AJI
2740 ''' </remarks>'
2741 Function M% fnTorqueCheck
2742     '�g���N�`�F�b�N�����M  �����n��~
2743     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2744     '
2745     fnTorqueCheck = 0
2746     Ovrd 20
2747     Mov PInitialPosition              '�����ʒu�ړ�
2748     Ovrd 100
2749     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2750     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2751     Dly 0.2
2752     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2753     '
2754     'M6340  �g���N�`�F�b�N��M
2755     'Dly 5.0
2756     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
2757     Dly 1.0
2758     M_Out(12340) = 0
2759     '
2760     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
2761     '
2762     MLoopFlg = 1
2763     While MLoopFlg = 1
2764         '
2765         Mov PInitialPosition              '�����ʒu�ړ�
2766         '
2767         MKeyNumber = fnKEY_WAIT()
2768         Select MKeyNumber
2769             Case Is = 1           '��~
2770                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
2771                 Dly 1.0
2772                 M_Out(12343) = 0
2773                 Ovrd 20
2774                 Mov PTicketRead_1
2775                 Ovrd 100
2776                 M_20# = 1
2777                 MLoopFlg = -1
2778                 Break
2779             Case Is = 2           '����
2780                 Break
2781             Case Is = 3           '�p��
2782                 Break
2783             Case Is = 4           '�g���N�`�F�b�N�J�n
2784                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
2785                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
2786                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2787                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2788                 MRet = fnMoveTorquePosi()
2789                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
2790                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2791                 Break
2792             Default
2793                 Break
2794         End Select
2795     WEnd
2796     '
2797     '�g���N�`�F�b�N����~���M
2798     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2799     '
2800     '���{�b�g�̈ʒu�����ɖ߂�
2801     '
2802     Exit Function
2803  FEnd
2804  '
2805 '
2806 '
2807 '---------------------------
2808 '
2809 '    ���C����ʂ̕\���A��\���ݒ�
2810 '         �R�����gD1001, D1002, D1003�̐ݒ�
2811 '           MWindReSet = 0     ��ʔ�\��
2812 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2813 '           MWindErrScr = 10    �G���[��� D1001, D1002
2814 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2815 '
2816 '---------------------------
2817 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2818     fnMainScreenOpen = 0
2819     '
2820    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2821         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2822     EndIf
2823     '
2824     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2825         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2826     EndIf
2827     '
2828     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2829         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2830     EndIf
2831     '
2832     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2833     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
2834     Dly 0.5
2835     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
2836     Exit Function
2837 FEnd
2838 '
2839 '��Main
2840 ''' <summary>
2841 ''' �g���N�`�F�b�N������
2842 ''' </summary>
2843 ''' <remarks>
2844 ''' Date   : 2021/12/21 : H.AJI
2845 ''' </remarks>'
2846 Function M% fnMoveTorquePosi
2847      fnMoveTorquePosi = 0
2848      Ovrd 50
2849      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
2850     '
2851     Spd M_NSpd
2852 '-------------      �h���C�o�[RST
2853     M_Out(12240)=0     '�h���C�o�[OFF CCW
2854     M_Out(12241)=0     '�h���C�o�[OFF CW
2855     M_Out(12242)=1     '�h���C�o�[���� C1
2856     M_Out(12243)=1     '�h���C�o�[���� C2
2857     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
2858 '---------------------------------------
2859 '[P-11]
2860 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
2861     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
2862     Dly 0.1
2863 '-----------------------
2864    'Cnt 0                           'Cnt����-2�@�I��
2865 '-----------------------
2866     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
2867     Dly 0.2
2868 '-----------------------
2869     ProgramBankSet(1,3)
2870     M_Out(12241)=0                   '�h���C�o�[OFF  CW
2871     'Dly 0.1
2872 '--------------------------------
2873     Ovrd 40
2874    'Dly 0.1
2875 '--------------------------------  �l�W���ߑ��x�ݒ�
2876     Spd 14                            '���C�h 100-40 100% :Spd 12
2877     Dly 0.1
2878 '--------------------------------
2879 '--------------------------------
2880 '---------------------------------�y�˂����ߓ���z
2881 '
2882     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2883    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2884     Dly 0.3                          '�������҂�
2885    M_Out(12241)=1                   '�h���C�o�[ON  CW
2886 '
2887     Wait M_In(11584)=1                '����/�G���[���o
2888     Dly 0.1
2889     Spd M_NSpd
2890    'Ovrd 20
2891     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2892     Wait M_In(11257)=1                '�l�W����SC
2893 '---------------------------------
2894     Dly 0.1
2895     M_Out(12241)=0                    '�h���C�o�[OFF CW
2896     Dly 0.1
2897     M_Out(12242)=0                    '�h���C�o�[���� C1
2898     Dly 0.1
2899     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2900     Dly 0.1
2901     M_Out(12245)=0                    '�v���O����2���� F1
2902 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2903 '
2904     Mvs PTorqueCheck,-60                       '������mov ����ύX
2905     Dly 0.1
2906 '--------------------------------------------------------------
2907    'Ovrd 80
2908 '--------------------------------------------------------------
2909 '---------------------------------------
2910 '---------------------------------------
2911 '---------------------------------------�G���[���E����
2912    *LBL1
2913    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2914    Mvs ,-100
2915    M_Out(12241)=0     '�h���C�o�[OFF CW
2916    Dly 0.1
2917    M_Out(12242)=0     '�h���C�o�[���� C1
2918    Dly 0.1
2919    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2920    Dly 0.1
2921    M_Out(12245)=0     '�v���O�������� F1
2922 '---------------------------------------
2923 '---------------------------------------
2924 '-------------
2925    'Mov PInitPos19049
2926    Dly 0.1
2927 '
2928 '
2929     Exit Function
2930 FEnd
2931 '
2932 '��Main
2933 ''' <summary>
2934 ''' �g������p�̃��C��
2935 ''' </summary>
2936 ''' <remarks>
2937 ''' Date   : 2021/07/07 : M.Hayakawa
2938 ''' </remarks>'
2939 Function Main
2940     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2941     '
2942     If M_Svo=0 Then
2943         Servo On
2944     EndIf
2945     Wait M_Svo=1
2946 '�g���X�^�[�g���t�����v���p���XON
2947     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2948 '�p�g���C�g����
2949     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2950     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2951     '
2952     M_20# = 0                                   'KEY���͏�����
2953     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
2954     MRet% = 0
2955 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2956     PActive = P_Curr                    '���݈ʒu���擾
2957     MRecoveryPass% = 0
2958     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2959         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2960             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2961             MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2962         EndIf
2963     EndIf
2964     EndIf
2965     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2966         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2967             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2968                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2969             EndIf
2970         EndIf
2971     EndIf
2972     If MRecoveryPass% = 0 Then
2973         fnInitialZone()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2974     EndIf
2975     '
2976     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2977         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2978 '�g���N�`�F�b�N
2979         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2980             MRet% = fnTorqueCheck()
2981             Break
2982         Else
2983 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
2984 '                MRtn = InspInit()               '�摜��������������
2985 '            EndIf
2986             '
2987            M_20# = MClear%                    '������
2988 '�g���J�n
2989             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2990                 MRet% = fnAssyStart()
2991             Else
2992                 M_20# = MPass%
2993             EndIf
2994 '�g���I�����t����
2995             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2996             Wait M_In(11572) = 1            '���t�擾����
2997             Dly 0.1
2998             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2999 '���t�^�[���j�b�g�ւ�OUT
3000             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3001             fnAutoScreenComment(89)         'AUTO��� �g����������
3002             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3003 'OK/NG�t���O�o��
3004             If M_20# <= 0 Then
3005                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3006             ElseIf M_20# = MPass% Then
3007                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3008             EndIf
3009 'PIAS�ɑg������������
3010             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3011                 If M_20# = MPass% Then
3012                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3013                 Else
3014                     'KEY���͂�NG�̏ꍇ
3015                     If M_20# = MNgProcess% Then
3016                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3017                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3018                         MRet% = fnPiasWrite(MNG%)
3019                        nAssyNgQty = nAssyNgQty + 1
3020                     EndIf
3021                     '
3022                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/07����)
3023                     If M_20# = MAssyOK% Then
3024                             '-----------------------
3025                             'D732 -> D2600 �R�s�[�v��
3026                             M_Out(12566) = 1
3027 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3028                             M_Out(12566) = 0
3029                             '
3030                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3031                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3032                             '��ԍ��ƍ�(PP�͖��g�p�j
3033 '                            MRet% = fnPCBNumberCheck()
3034                         Else
3035                             MRet% = 1
3036                         EndIf
3037                         '
3038                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3039                             If M_20# <> MAbout% Then
3040                                 '�H������OK��������
3041                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3042                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3043                                 MRet% = fnPiasWrite(MOK%)
3044                                 nAssyOkQty = 0
3045                                 nAssyOkQty = nAssyOkQty + 1
3046                             Else
3047                                 nAssyOkQty = nAssyOkQty + 1
3048                             EndIf
3049                         EndIf
3050                     EndIf
3051 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3052 '                    MRet% = fnPiasWrite(MOK%)
3053                 EndIf
3054             Else
3055                 nAssyOkQty = nAssyOkQty + 1
3056             EndIf
3057             '
3058             '�g���I�����t��������
3059             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3060             '�������A�g��OK���A�g��NG��������
3061 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3062             '
3063 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3064 '                '�摜�����I������
3065 '                MRtn = InspQuit()
3066 '            EndIf
3067         EndIf
3068         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3069     EndIf
3070 '�p�g���C�g����
3071     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3072     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3073 'GOT�\��
3074     fnAutoScreenComment(93)  'AUTO��� �H������
3075 FEnd
3076 End
3077 '
3078 '���܂��Ȃ��R�����g
3079 '��΍폜�����
3080 '
3081 '
3082 '
3083 '
PInspPosition(1)=(+343.72,-16.25,+435.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(11)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(12)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(13)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(14)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(15)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(16)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(17)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(18)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(19)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(20)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(21)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(22)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(23)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(24)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(25)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(26)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(27)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(28)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(29)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(30)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTemp=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(+320.61,-172.83,+395.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(2)=(+320.61,-172.83,+345.08,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+320.61,-172.83,+339.08,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPos(1)=(+180.56,+239.25,+380.00,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(2)=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(10)=(+180.56,+239.25,+339.00,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PEscapePosi(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PActive=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
Pmove=(+603.00,-149.18,+380.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
PInitialPosition=(+250.00,+0.00,+450.00,+180.00,+0.00,+180.00)(7,0)
PScrewSoc1=(+299.38,-64.81,+337.50,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_0=(+299.38,-64.81,+343.50,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_1=(+299.38,-64.81,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2=(+320.71,-24.83,+337.59,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_0=(+320.71,-24.83,+343.59,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_1=(+320.71,-24.83,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3=(+380.77,-25.04,+338.79,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_0=(+380.77,-25.04,+344.79,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_1=(+380.77,-25.04,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4=(+391.11,-85.99,+338.72,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_0=(+391.11,-85.99,+344.72,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_1=(+391.11,-85.99,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5=(+370.61,-153.98,+338.92,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_0=(+370.61,-153.98,+344.92,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_1=(+370.61,-153.98,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6=(+320.61,-172.83,+339.08,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_0=(+320.61,-172.83,+345.08,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_1=(+320.61,-172.83,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupply=(+180.56,+239.25,+339.00,+180.00,+0.00,-120.00)(7,0)
PScrewSupply_1=(+180.56,+239.25,+380.00,+180.00,+0.00,-120.00)(7,0)
PScrewSupply_2=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00)(7,0)
PScrewSupply_9=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00)(7,0)
PSocCheck=(+325.25,-60.87,+444.00,+180.00,-0.01,-180.00)(7,0)
PSocCheck_1=(+325.25,-60.87,+470.00,+180.00,-0.01,-180.00)(7,0)
PSocGet=(+627.82,+106.92,+311.65,-179.93,+0.04,-179.12)(7,0)
PSocGet_1=(+627.82,+106.92,+330.00,-179.93,+0.04,-179.12)(7,0)
PSocGet_2=(+627.82,+106.92,+380.00,-179.93,+0.04,-179.12)(7,0)
PSocPcbRead=(+343.72,-16.25,+435.00,-180.00,+0.00,-180.00)(7,0)
PSocPcbRead_1=(+343.72,-16.25,+480.00,-180.00,+0.00,-180.00)(7,0)
PSocPress=(+391.85,+11.73,+365.50,-180.00,+0.00,+180.00)(7,0)
PSocPress_1=(+391.85,+11.73,+380.00,-180.00,+0.00,+180.00)(7,0)
PSocPress_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PSocSet=(+486.15,-97.80,+352.52,+180.00,-0.04,-179.20)(7,0)
PSocSet_1=(+486.15,-97.80,+361.91,+180.00,-0.04,-179.20)(7,0)
PSocSet_2=(+486.15,-97.80,+380.00,+180.00,-0.04,-179.20)(7,0)
PTicketRead=(+603.00,-149.18,+373.00,-179.99,+0.00,+90.00)(7,0)
PTicketRead_1=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00)(7,0)
PTorqueCheck=(+144.46,-240.78,+340.00,-179.99,-0.01,+90.02)(7,0)
PTorqueCheck_1=(+144.45,-240.80,+360.00,-179.99,+0.00,+90.01)(7,0)
