1 ' ===================================
2 '
3 '  21049001 STEP5 Assy1�v���O����
4 '
5 ' �쐬�ҁF������T
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
43 Def Inte MovrdA                     '�l�W����Ovrd �ϗp   20191127�ǉ�
44 Def Float MSpdA                     '�l�W����Spd�@�ϗp   20191127�ǉ�
45 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p    20200312�ǉ�'
46 MovrdA% = 20                        '�l�W����Ovrd �ϗp   20191127�ǉ�
47 MSpdA = 800                        '�l�W����Spd�@�ϗp   20191127�ǉ�
48 '===== <Insight�ϐ��ݒ�> =====
49 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
50 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
51 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
52 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
53 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
54 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
55 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
56 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
57 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
58 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
59 'Output Signal
60 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
61 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
62 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
63 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
64 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
65 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
66 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
67 '===== <�d�h���萔> =====
68 '===== <�d�h���ϐ���`> =====
69 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
70 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
71 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
72 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
73 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
74 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
75 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
76 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
77 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
78 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
79 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
80 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
81 Y60_Driver=12240 '�d�h�������v��� CCW
82 Y61_Driver=12241 '�d�h�����v��� CW
83 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
84 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
85 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
86 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
87 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
88 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
89 '�g��2
90 X34_NG1=11268 '�˂�����1�@Read
91 X35_NG2=11269 '�˂�����2�@Read
92 '�g��3
93 X3F_NG1=11279 '�˂�����1�@Read
94 '
95 Dim PScrewPosTemp(10)                                               '�l�W���ߗpFunction�����ϐ�
96 Dim PGetScrewPosTemp(10)                                            '�˂������@����˂��𓾂�Function�����ϐ�
97 Dim PEscapePosi(10)
98 MLoopCnt% = 0'
99 '===== <���{�b�g�萔> =====
100 '===== <���{�b�g�ϐ���`> =====
101 MRBTOpeGroupNo = 0                    '���{�b�g����ԍ�������
102 MCommentD1001 = 0
103 MCommentD1002 = 0
104 MCommentD1003 = 0
105 MScreenNo = 0
106 '
107 MCommentTSU = 0
108 MCommentTSD = 0
109 '�E�B���h��ʔԍ��ݒ�
110 MWindReSet = 0
111 MWindInfoScr = 5
112 MWindErrScr = 10
113 MWindErrScr2 = 11
114 MWindErrScr3 = 13
115 MWindErrScr17 = 17
116 MWindErrScr18 = 18
117 MWindCmmnScr = 20
118 MWindJigRelase19049 = 60
119 MWindJigRelase19050 = 61
120 MWindJigRelase19051 = 62
121 '
122 MClear% = 0        'KEY_�̃N���A
123 MAbout% = 1        'KEY_��~
124 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
125 MContinue% = 3     'KEY_�p�� �ēx����������s��
126 '
127 Def Inte MNgProcess
128 MNgProcess% = 5      'KEY_NG
129 '
130 MAssyOK% = 6       '�g������
131 MPass% = 7         '�H���p�X
132 MPiasNG% = 8       'Pias�m�F������NG
133 '
134 '�������pKEY�ԍ�   '
135 MRobotInit1% = 11  '�����ʒu�p
136 MRobotInit2% = 12  '�����ʒu�p
137 MRobotInit3% = 13  '�����ʒu�p
138 MRobotInit4% = 14  '�����ʒu�p
139 '
140 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
141 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
142 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
143 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
144 '
145 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
146 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
147 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
148 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
149 '
150 MopeNo = 0
151 '
152 MOK% = 1               '�e����p
153 MNG% = 0               '�e����p
154 MTIMEOUT% = -1         '�e����p
155 MJudge% = 0            '������i�[�p
156 '
157 '
158 MRECIVETIME& = 0
159 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
160 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
161 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
162 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
163 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
164 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
165 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
166 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
167 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
168 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
169 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
170 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
171 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
172 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
173 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
174 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
175 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
176 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
177 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
178 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
179 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
180 MIN_PIAS_MyProcessComp% = 11573        '���H����������
181 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
182 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
183 '
184 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
185 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
186 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
187 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
188 '
189 MOUT_PiasAssyResultOK% = 12549    '�g��OK
190 MOUT_PiasAssyResultNG% = 12550    '�g��NG
191 MOUT_PiasAssyResultWr% = 12548    '�H��������������
192 '
193 MIN_PiasProcessNG% = 11559        '�H����������NG
194 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
195 MIN_PiasProcessOK% = 11558        '�H����������OK
196 '
197 MIN_Insight_Use% = 11369               '�摜�m�FON
198 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
199 '
200 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
201 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
202 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
203 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
204 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
205 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
206 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
207 '
208 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
209 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
210 '
211 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
212 '
213 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
214 '
215 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
216 MRtn% = 0
217 MopeNo = 0
218 MRet = 0
219 'MRtn = 0
220 MRet3% = 0
221 '
222 Def Inte MInputQty          '������ ���Z�ϐ�
223 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
224 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
225 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
226 Def Inte nAssyOkQty         '���g�p
227 Def Inte MScrewNo
228 Def Inte MReTry
229 '===== <IO�ϐ���`> =====
230 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
231 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
232 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
233 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
234 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
235 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
236 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
237 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
238 '
239 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
240 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
241 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
242 '
243 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
244 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
245 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
246 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
247 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
248 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
249 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
250 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
251 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
252 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
253 '
254 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
255 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
256 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
257 '
258 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
259 '
260 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
261 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
262 '
263 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
264 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
265 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
266 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
267 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
268 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
269 '
270 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
271 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
272 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
273 '
274 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
275 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
276 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
277 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
278 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
279 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
280 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
281 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u 'Y68_VV1% = 12250��Y68_VV1% = 12248�ɕύX(8/27����)
282 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u'Y6B_VB1% = 12251��Y6B_VB1% = 12250�ɕύX(8/27����)
283 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
284 '
285 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
286 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
287 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
288 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
289 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
290 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
291 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
292 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
293 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
294 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
295 '
296 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
297 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
298 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
299 '
300 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
301 '
302 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
303 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
304 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
305 '
306 '
307 '����
308 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
309 Def Inte MOn                            '�o��=1
310 Def Inte MOff                           '�o��=0
311 '
312 '�˂����ߑ��u_�o�̓A�h���X
313 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
314 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
315 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
316 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
317 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
318 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
319 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
320 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
321 '�˂����ߑ��u_���̓A�h���X
322 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
323 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
324 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
325 Def Inte MIN_ScwT_Case1                 '����1��~����M
326 Def Inte MIN_ScwT_Case2                 '����2��~����M
327 Def Inte MIN_ScwT_Case3                 '����3��~����M
328 Def Inte MIN_ScwT_Case4                 '����4��~����M
329 Def Inte MIN_ScwT_Case5                 '����5��~����M
330 '
331 '
332 Def Inte MRetryLimit                    ' ���g���C��
333 Def Inte MRetryCount                    ' ���g���C�J�E���g
334 '
335 Dim MScwT_Case1%(2)               '����1��~�ϐ�
336 Dim MScwT_Case2%(2)               '����2��~�ϐ�
337 Dim MScwT_Case3%(2)               '����3��~�ϐ�
338 Dim MScwT_Case4%(2)               '����4��~�ϐ�
339 Dim MScwT_Case5%(2)               '����5��~�ϐ�
340 Def Pos PActive                     '�������W�n �ʒu�ϐ� ���݈ʒu
341 Def Pos Pmove                       '�������W�n �ʒu�ϐ� �ړ���
342 Def Inte MRecoveryPass              '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s'
343 '����
344 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
345 MOn% = 1                                 '�o�� = 1
346 MOff% = 0                                '�o�� = 0
347 '
348 '�˂����ߋ@_�A�h���X�ݒ�
349 MOUT_ScwT_ComChk% = 12816               '�ʐM�m�F���M
350 MOUT_ScwT_ST% = 12849                   '�˂����ߊJ�n�𑗐M
351 MOUT_ScwT_ReSTOK% = 12850               '�ĊJ�n��M�𑗐M
352 MOUT_ScwT_FinOK% = 12852                '�˂����ߊ�����M�𑗐M
353 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
354 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
355 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
356 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
357 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
358 '
359 MIN_ScwT_comOK% = 11824                 '�˂����ߑ��u����ԐM
360 MIN_ScwT_STRec% = 11857                 '�˂����ߊJ�n����M
361 MIN_ScwT_ReST% = 11858                  '�ĊJ�n����M
362 MIN_ScwT_Fin% = 11860                   '�˂����ߊ�������M
363 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
364 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
365 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
366 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
367 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
368 '
369 MScwT_Case1%(1) = MIN_ScwT_Case1%
370 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
371 MScwT_Case2%(1) = MIN_ScwT_Case2%
372 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
373 MScwT_Case3%(1) = MIN_ScwT_Case3%
374 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
375 MScwT_Case4%(1) = MIN_ScwT_Case4%
376 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
377 MScwT_Case5%(1) = MIN_ScwT_Case5%
378 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
379 '
380 MRetryLimit% = 2
381 '
382 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
383 Function M% fnAssyStart
384     M_20# = MClear%                       '������
385     '�g�ݗ��ĊJ�n
386     '�v���O�������_
387     Ovrd 100
388     ' �����ʒu��ID�`�P�b�g��Ƃ��邽�ߍ폜 9/16 M.Hayakawa
389 '    Mov PInitialPosition        '���_���
390     '�����ʒu��ݒ�
391     PTemp = P_Curr
392     MRtn = 0
393     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
394         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
395             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
396                 MRtn = 1
397                 Break
398             EndIf
399             Break
400         EndIf
401         Break
402     EndIf
403     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
404     If MRtn = 1 Then
405         M_Out(12256) = 1 Dly 0.3    '�ʒu���ߏoON
406         Mov PTicketRead
407         Break
408     Else
409         Mov PInitialPosition
410         M_Out(12256) = 1 Dly 0.3    '�ʒu���ߏoON
411         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
412         Mvs PTicketRead             'ID�ǂ݈ʒu
413         Break
414     EndIf
415     *RE_PUSH
416 '    If M_20# = MContinue% Then M_Out(12257) = 0
417     If M_20# = MContinue% Then M_Out(12256) = 1 Dly 0.3
418     If M_20# = MContinue% Then M_20# = MClear%
419     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
420     If MRtn = 1 Then GoTo *CompPush
421 '    M_Out(12256) = 0 Dly 0.3
422     M_Out(12257) = 1 Dly 0.3
423     fErrorProcess(11,231,282,0)
424     If M_20# = MNext% Then M_20# = MClear%
425     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
426     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
427     If M_20# = MContinue% Then GoTo *RE_PUSH
428     *CompPush
429     '
430     *RE_READ
431     If M_20# = MContinue% Then M_20# = MClear%
432 '
433     MRtn = 1                            'MRtn������
434     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
435         MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
436     EndIf
437         '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
438         '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
439 '
440     If MRtn = 1 Then GoTo *CompRead
441     Mvs PTicketRead_1                       ' ��U���ɑҔ� �ǉ� 22/07/16 M,H
442     'fErrorProcess(11,97,25,0)
443     If M_20# = MPass% Then GoTo *AssyEnd    ' ���ւ������ꂽ���̃R�����g�����{�W�����v��ύX 2022/07/20 M.H
444     If M_20# = MNext% Then M_20# = MClear%
445     If M_20# = MAbout% Then GoTo *AssyEnd       ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
446     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
447     If M_20# = MContinue% Then GoTo *RE_READ
448 '    If M_20# = MNext% Then M_20# = MPass%
449     GoTo *ASSY_ERROR_END
450     *CompRead
451     '
452 '�yMAIN���ID�ǂݍ��݁z
453     *RE_MEIN_CHECK
454     PInspPosition(1) = PMainPcbRead 'MAIN��Ǎ��ʒu
455     MInspGroup%(1) = 2              '����G�ԍ�
456     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
457 '
458     If MRtn = 1 Then GoTo *CompMainCheck
459     fErrorProcess(11,38,25,0)
460     If M_20# = MNext% Then M_20# = MClear%
461     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
462     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
463     If M_20# = MContinue% Then GoTo *RE_MEIN_CHECK
464     *CompMainCheck
465 '�yGYRO���ID�ǂݍ��݁z
466     *RE_GYRO_CHECK
467     PInspPosition(1) = PGyroPcbRead 'GYRO��Ǎ��ʒu
468     MInspGroup%(1) = 3              '����G�ԍ�
469     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
470 '
471     If MRtn = 1 Then GoTo *CompGyroCheck
472     fErrorProcess(11,38,25,0)
473     If M_20# = MNext% Then M_20# = MClear%
474     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
475     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
476     If M_20# = MContinue% Then GoTo *RE_GYRO_CHECK
477     *CompGyroCheck
478 '�y���ID�R�s�[�z
479     *RE_PCB_RECORD
480     M_Out(12571) = 1    ' �̈�1 ��ԍ��R�s�[ (D2600-) On
481     Dly 0.1
482     M_Out(12572) = 1    ' �̈�2 ��ԍ��R�s�[ (D2612-) On
483     Dly 0.1
484     M_Out(12566) = 1    ' toPLC_��ԍ��R�s�[�v�� On
485 '
486     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��R�s�[���� On
487     If MRtn = 1 Then
488         M_Out(12571) = 0  ' �̈�1 ��ԍ��R�s�[ (D2600-) Off
489         Dly 0.1
490         M_Out(12572) = 0  ' �̈�2 ��ԍ��R�s�[ (D2612-) Off
491         Dly 0.1
492         M_Out(12566) = 0  ' toPLC_��ԍ��R�s�[�v�� Off
493 '        GoTo *RE_PCB_COMPAIRE   ' ��ԍ��ƍ��ɃX�L�b�v
494     Else
495         fErrorProcess(11,39,25,0)
496         If M_20# = MNext% Then M_20# = MClear%
497         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
498         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
499         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
500     EndIf
501 '�y���ID�ƍ��i�R�t���j�z
502     MRetryCount% = 0
503     While (MRetryCount% <= MRetryLimit%)
504         *RE_PCB_COMPAIRE
505         M_Out(12557)= 1 ' ��ԍ��ƍ��r�b�gON
506         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��ƍ�OK(M420) On
507         If MRtn = 1 Then
508             M_Out(12557)= 0     ' ��ԍ��ƍ��r�b�gOff
509             ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
510             MRetryCount% = 99
511         Else
512             If MRetryCount% = MRetryLimit% Then
513                 If M_In(11565) = 1 Then
514                     fErrorProcess(11,37,25,0)
515                 Else
516                     fErrorProcess(11,38,25,0)
517                 EndIf
518                 If M_20# = MNext% Then
519                     M_20# = MClear%
520                     ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
521                     MRetryCount% = 99
522                 EndIf
523                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
524                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
525                 If M_20# = MContinue% Then
526                     MRetryCount% = 0
527                 EndIf
528             Else
529                 ' ���g���C�񐔃C���N�������g
530                 MRetryCount% = MRetryCount% + 1
531                 Dly 0.1  ' ���̍H���ƃ^�C�~���O�����炷�ׂ̃f�B���C
532             EndIf
533         EndIf
534     WEnd
535 '
536     *RE_CHECK
537     PInspPosition(1) = PParts1Check '���i1�摜�`�F�b�N�ʒu�i�{��1-1�j
538     MInspGroup%(1) = 4              '����G�ԍ�
539     PInspPosition(2) = PParts5Check '���i5�摜�`�F�b�N�ʒu�i�{��1-2�j
540     MInspGroup%(2) = 8              '����G�ԍ�
541     PInspPosition(3) = PParts2Check '���i2�摜�`�F�b�N�ʒu
542     MInspGroup%(3) = 5              '����G�ԍ�
543     PInspPosition(4) = PParts3Check '���i3�摜�`�F�b�N�ʒu
544     MInspGroup%(4) = 6              '����G�ԍ�
545     PInspPosition(5) = PParts4Check '���i4�摜�`�F�b�N�ʒu
546     MInspGroup%(5) = 7              '����G�ԍ�
547     PInspPosition(6) = PParts6Check '���i6�摜�`�F�b�N�ʒu(���ʒu2�摜�ۑ��p)
548     MInspGroup%(6) = 9              '����G�ԍ�
549     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 6, -1, 1 )  '�摜�����������s
550     If MRtn = 1 Then GoTo *CompCheck
551       ' �b��폜
552     fErrorProcess(11,43,46,0)
553     If M_20# = MNext% Then M_20# = MClear%
554     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
555     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
556     If M_20# = MContinue% Then GoTo *RE_CHECK
557     *CompCheck
558     ' ���i�����ǉ� 9/16 M.Hayakawa
559     ' ���i����1
560 '    Mvs PParts1Check             '���i1����
561     ' ���i����2
562 '    Mvs PParts2Check             '���i2����
563     ' ���i����3
564 '    Mvs PParts3Check             '���i3����
565     ' ���i����4
566 '    Mvs PParts4Check             '���i4����
567 '
568     'Main���ID��ǂ�
569 '    Mvs PMainPcbRead            'MAIN���ID�ǎ��
570     '
571     'GYRO���ID��ǂ�
572 '    Mvs PGyroPcbRead            'GYRO���ID�ǂݎ��
573     '
574     '���i�ʒu����(ID�Ǎ���ɕύX 9/16 M.Hayakawa�j
575     *RE_POS
576     If M_20# = MContinue% Then M_20# = MClear%
577     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
578     M_Out(12256)=1 Dly 0.3      '�ʒu����CY�pSV�o�[�p���X�o��
579     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
580 '
581     'Wait M_In(11266)=1          '�ʒu���ߏo�[���o�C���ɂ��R�����g�A�E�g(8/26����))
582     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
583     If MRtn = 1 Then GoTo *Comp_Pos_1
584     fErrorProcess(11,231,282,0)
585     If M_20# = MNext% Then M_20# = MClear%
586     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
587     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
588     If M_20# = MContinue% Then GoTo *RE_POS
589     *Comp_Pos_1
590     '
591     M_Out(12258)=1 Dly 0.3      '�v�b�V��CY�pSV�o�[�p���X�o��(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
592     M_Out(12260)=1 Dly 0.3      'FAN�N�����v�ߒ[�p���X�o��(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
593     Mov PScrewSupplyMain_1
594 '
595 '    M_Out(12258)=1 Dly 0.3      '�v�b�V��CY�pSV�o�[�p���X�o��
596     'Wait M_In(11268)=1          '�v�b�V���o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
597     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '�v�b�V���o�[���o
598     If MRtn = 1 Then GoTo *Comp_Pos_2
599     fErrorProcess(11,231,282,0)
600     If M_20# = MNext% Then M_20# = MClear%
601     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
602     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
603     If M_20# = MContinue% Then GoTo *RE_POS
604     *Comp_Pos_2
605     '
606 '    M_Out(12260)=1 Dly 0.3      'FAN�N�����v�ߒ[�p���X�o��(���C���˂����ߌ�ɕύX M.Hayakawa)(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
607     'Wait M_In(11270)=1          'FAN�N�����v�ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
608     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'FAN�N�����v�ߒ[���o(8/26����)
609     If MRtn = 1 Then GoTo *Comp_Pos_3
610     fErrorProcess(11,231,282,0)
611     If M_20# = MNext% Then M_20# = MClear%
612     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
613     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
614     If M_20# = MContinue% Then GoTo *RE_POS
615     *Comp_Pos_3
616     '
617     '
618     'Main��̃l�W����
619     'Main��p�l�W�����@�փl�W�����ɍs��
620     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
621     '
622     '*ScrewSupplyMain           '�ꎞ�R�����g�A�E�g(�ȉ�5�s,8/5����)
623 '    Mov PScrewSupplyMain_2      '�l�W�����@���_
624 '    Mov PScrewSupplyMain_1      '�l�W�s�b�N�A�b�v���
625 '    Mvs PScrewSupplyMain        '�l�W�s�b�N�A�b�v
626 '    Mvs PScrewSupplyMain_1      '�l�W�s�b�N�A�b�v���
627 '    Mov PScrewSupplyMain_2      '�l�W�����@���_
628     'Return                     '�ꎞ�R�����g�A�E�g(8/4����)
629     'ScrewPositionDebug_1()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
630     '
631     PGetScrewPosTemp(1) = PScrewSupplyMain_1   '�l�W�s�b�N�A�b�v������(8/26����)
632     PGetScrewPosTemp(2) = PScrewSupplyMain_2   '�l�W�������_����(8/26����)
633     PGetScrewPosTemp(9) = PScrewSupplyMain_9   '�l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
634     PGetScrewPosTemp(10) = PScrewSupplyMain    '�l�W�s�b�N�A�b�v����(8/26����)
635     *RE_SCREW_GET_1                                '���g���C�p���x��
636     If M_20# = MContinue% Then M_20# = MClear%
637     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
638     If M_20# = MClear% Then GoTo *Comp_Screw_1
639     If M_20# = MNext% Then M_20# = MClear%
640     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
641     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
642     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
643     *Comp_Screw_1
644     '
645     '�@�ԃl�W����
646 '    Mov PScrewMain1_1           '�@���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
647 '    Ovrd 5
648 '    Mvs PScrewMain1             '�@�l�W����
649 '    Ovrd 10
650 '    Mvs PScrewMain1_1           '�@���
651     PScrewPosTemp(1) = PScrewMain1_1    '�l�W1���ߊJ�n�ʒu������(8/26����)
652     PScrewPosTemp(2) = PScrewMain1_0    '�l�W1���ߊJ�n�ʒu����(8/26����)
653     PScrewPosTemp(10) = PScrewMain1     '�l�W1���ߏI���ʒu����(8/26����)
654     M_Out16(12672) = 1              '�l�W���߈ʒu�ԍ����M
655     MRtn = ScrewTight(PScrewPosTemp,1,10.0)    '�l�W1���߂̎��s(8/26����) 4.9�ɕύX20220205��
656     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
657     If MRtn = 1 Then GoTo *CompScrew1
658     Mov PInitialPosition
659     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
660     MScrewErrorCord% = MScrewErrorCord% + 1
661     fErrorProcess(11,MScrewErrorCord%,52,0)
662 '    fErrorProcess(11,53,52,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
665     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
666     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
667     *CompScrew1
668     '
669     'Main��p�l�W�����@�փl�W�����ɍs��
670     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
671     'ScrewPositionDebug_1()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
672     *RE_SCREW_GET_2                                '���g���C�p���x��
673     If M_20# = MContinue% Then M_20# = MClear%
674     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
675     If M_20# = MClear% Then GoTo *Comp_Screw_2
676     If M_20# = MNext% Then M_20# = MClear%
677     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
678     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
679     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
680     *Comp_Screw_2
681     '�A�ԃl�W����
682 '    Mov PScrewMain2_1           '�A���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
683 '    Ovrd 5
684 '    Mvs PScrewMain2             '�A�l�W����
685 '    Ovrd 10
686 '    Mvs PScrewMain2_1           '�A���
687     PScrewPosTemp(1) = PScrewMain2_1    '�l�W2���ߊJ�n�ʒu������(8/26����)
688     PScrewPosTemp(2) = PScrewMain2_0    '�l�W2���ߊJ�n�ʒu����(8/26����)
689     PScrewPosTemp(10) = PScrewMain2     '�l�W1���ߏI���ʒu����(8/26����)
690     M_Out16(12672) = 2              '�l�W���߈ʒu�ԍ����M
691     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����2�̎��s(8/26����)
692     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
693     If MRtn = 1 Then GoTo *CompScrew2
694     Mov PInitialPosition
695     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
696     MScrewErrorCord% = MScrewErrorCord% + 2
697     fErrorProcess(11,MScrewErrorCord%,52,0)
698 '    fErrorProcess(11,54,52,0)
699     If M_20# = MNext% Then M_20# = MClear%
700     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
701     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
702     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
703     *CompScrew2
704     '
705     'Main��p�l�W�����@�փl�W�����ɍs��
706     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
707     *RE_SCREW_GET_3                                '���g���C�p���x��
708     If M_20# = MContinue% Then M_20# = MClear%
709     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
710     If M_20# = MClear% Then GoTo *Comp_Screw_3
711     If M_20# = MNext% Then M_20# = MClear%
712     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
713     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
714     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
715     *Comp_Screw_3
716     '�B�ԃl�W����
717 '    Mov PScrewMain3_1           '�B���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
718 '    Ovrd 5
719 '    Mvs PScrewMain3             '�B�l�W����
720 '    Ovrd 10
721 '    Mvs PScrewMain3_1           '�B���
722     PScrewPosTemp(1) = PScrewMain3_1    '�l�W3���ߊJ�n�ʒu������(8/26����)
723     PScrewPosTemp(2) = PScrewMain3_0    '�l�W3���ߊJ�n�ʒu����(8/26����)
724     PScrewPosTemp(10) = PScrewMain3     '�l�W3���ߏI���ʒu����(8/26����)
725     M_Out16(12672) = 3              '�l�W���߈ʒu�ԍ����M
726     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����3�̎��s(8/26����)
727     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
728     If MRtn = 1 Then GoTo *CompScrew3
729     Mov PInitialPosition
730     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
731     MScrewErrorCord% = MScrewErrorCord% + 3
732     fErrorProcess(11,MScrewErrorCord%,52,0)
733 '    fErrorProcess(11,55,52,0)
734     If M_20# = MNext% Then M_20# = MClear%
735     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
736     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
737     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
738     *CompScrew3
739 '
740     '
741     'Main��p�l�W�����@�փl�W�����ɍs��
742     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
743     *RE_SCREW_GET_4                                '���g���C�p���x��
744     If M_20# = MContinue% Then M_20# = MClear%
745     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
746     If M_20# = MClear% Then GoTo *Comp_Screw_4
747     If M_20# = MNext% Then M_20# = MClear%
748     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
749     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
750     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
751     *Comp_Screw_4
752     '�C�ԃl�W����
753 '    Mov PScrewMain4_1           '�C���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
754 '    Ovrd 5
755 '    Mvs PScrewMain4             '�C�l�W����
756 '    Ovrd 10
757 '    Mvs PScrewMain4_1           '�C���
758     PScrewPosTemp(1) = PScrewMain4_1    '�l�W4���ߊJ�n�ʒu������(8/26����)
759     PScrewPosTemp(2) = PScrewMain4_0    '�l�W4���ߊJ�n�ʒu����(8/26����)
760     PScrewPosTemp(10) = PScrewMain4     '�l�W4���ߏI���ʒu����(8/26����)
761     M_Out16(12672) = 4              '�l�W���߈ʒu�ԍ����M
762     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����4�̎��s(8/26����)
763     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
764     If MRtn = 1 Then GoTo *CompScrew4
765     Mov PInitialPosition
766     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
767     MScrewErrorCord% = MScrewErrorCord% + 4
768     fErrorProcess(11,MScrewErrorCord%,52,0)
769 '    fErrorProcess(11,56,52,0)
770     If M_20# = MNext% Then M_20# = MClear%
771     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
772     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
773     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
774     *CompScrew4
775     '
776     'Main��p�l�W�����@�փl�W�����ɍs��
777     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
778     *RE_SCREW_GET_5                                '���g���C�p���x��
779     If M_20# = MContinue% Then M_20# = MClear%
780     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
781     If M_20# = MClear% Then GoTo *Comp_Screw_5
782     If M_20# = MNext% Then M_20# = MClear%
783     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
784     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
785     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
786     *Comp_Screw_5
787     '�D�ԃl�W����
788 '    Mov PScrewMain5_1           '�D���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
789 '    Ovrd 5
790 '    Mvs PScrewMain5             '�D�l�W����
791 '    Ovrd 10
792 '    Mvs PScrewMain5_1           '�D���
793     PScrewPosTemp(1) = PScrewMain5_1    '�l�W5���ߊJ�n�ʒu������(8/26����)
794     PScrewPosTemp(2) = PScrewMain5_0    '�l�W5���ߊJ�n�ʒu����(8/26����)
795     PScrewPosTemp(10) = PScrewMain5     '�l�W5���ߏI���ʒu����(8/26����)
796     M_Out16(12672) = 5              '�l�W���߈ʒu�ԍ����M
797     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        '�l�W����5�̎��s(8/26����)  �o���N�ύX1��6�@�����@2/11
798     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
799     If MRtn = 1 Then GoTo *CompScrew5
800     Mov PInitialPosition
801     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
802     MScrewErrorCord% = MScrewErrorCord% + 5
803     fErrorProcess(11,MScrewErrorCord%,52,0)
804 '    fErrorProcess(11,57,52,0)
805     If M_20# = MNext% Then M_20# = MClear%
806     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
807     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
808     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
809     *CompScrew5
810     '
811     'Main��p�l�W�����@�փl�W�����ɍs��
812     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
813 '�ȉ�3�sPP�i�Ƀl�W�����Ȃ����߈ꎞ�폜 9/16 M.Hayakawa
814     *RE_SCREW_GET_6                                '���g���C�p���x��
815     If M_20# = MContinue% Then M_20# = MClear%
816     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
817     If M_20# = MClear% Then GoTo *Comp_Screw_6
818     If M_20# = MNext% Then M_20# = MClear%
819     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
820     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
821     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
822     *Comp_Screw_6
823     '�E�ԃl�W����
824 '    Mov PScrewMain6_1           '�E���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
825 '    Ovrd 5
826 '    Mvs PScrewMain6             '�E�l�W����
827 '    Ovrd 10
828 '    Mvs PScrewMain6_1           '�E���
829 '�ȉ�3�sPP�i�Ƀl�W�����Ȃ����߈ꎞ�폜 9/16 M.Hayakawa
830     PScrewPosTemp(1) = PScrewMain6_1    '�l�W6���ߊJ�n�ʒu������(8/26����)
831     PScrewPosTemp(2) = PScrewMain6_0    '�l�W6���ߊJ�n�ʒu����(8/26����)
832     PScrewPosTemp(10) = PScrewMain6     '�l�W6���ߏI���ʒu����(8/26����)
833     M_Out16(12672) = 6              '�l�W���߈ʒu�ԍ����M
834     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        '�l�W����6�̎��s(8/26����)�@�@�o���N�ύX1��6�@�����@2/11
835     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
836     If MRtn = 1 Then GoTo *CompScrew6
837     Mov PInitialPosition
838     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
839     MScrewErrorCord% = MScrewErrorCord% + 6
840     fErrorProcess(11,MScrewErrorCord%,52,0)
841 '    fErrorProcess(11,58,52,0)
842     If M_20# = MNext% Then M_20# = MClear%
843     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
844     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
845     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
846     *CompScrew6
847     '
848     'FAN�p�l�W�����@�փl�W�����ɍs��
849     'GoSub *ScrewSupplyFan      '�ꎞ�R�����g�A�E�g(8/4����)
850 ' �l�W�ʒu�w��O�Ɏ��ɂ����Ă���H 1�s�ꎞ�폜 9/16 M.Hayakawa
851 '    MRtn = ScrewGet(PGetScrewPosTemp)       '�l�W�����ɍs��(8/26����)
852     '
853 '    *ScrewSupplyFan
854 '    Mov PScrewSupplyFan_2       '�l�W�����@���_
855 '    Mov PScrewSupplyFan_1       '�l�W�s�b�N�A�b�v���
856 '    Mvs PScrewSupplyFan         ''�l�W�s�b�N�A�b�v
857 '    Mvs PScrewSupplyFan_1       '�l�W�s�b�N�A�b�v���
858 '    Mov PScrewSupplyFan_2       '�l�W�����@���_
859    ' Return                     '�ꎞ�R�����g�A�E�g(8/4����)
860     'ScrewPositionDebug_2()       '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
861     '
862     PGetScrewPosTemp(1) = PScrewSupplyFan_1   '�l�W�s�b�N�A�b�v������(8/26����)
863     PGetScrewPosTemp(2) = PScrewSupplyFan_2   '�l�W�������_����(8/26����)
864     PGetScrewPosTemp(9) = PScrewSupplyFan_9   '�l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
865     PGetScrewPosTemp(10) = PScrewSupplyFan    '�l�W�s�b�N�A�b�v����(8/26����)
866     *RE_SCREW_GET_7                                '���g���C�p���x��
867     If M_20# = MContinue% Then M_20# = MClear%
868     ScrewGet(PGetScrewPosTemp , 11260 , 0)          '�l�W�󂯎��J�n
869     If M_20# = MClear% Then GoTo *Comp_Screw_7
870     If M_20# = MNext% Then M_20# = MClear%
871     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
872     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
873     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
874     *Comp_Screw_7
875     '�F�ԃl�W����
876 '    Mov PScrewFan1_1            '�F���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
877 '    Ovrd 5
878 '    Mvs PScrewFan1              '�F�l�W����
879 '    Ovrd 10
880 '    Mvs PScrewFan1_1            '�F���
881     PScrewPosTemp(1) = PScrewFan1_1    'Fan1�l�W���ߊJ�n�ʒu������(8/26����)
882     PScrewPosTemp(2) = PScrewFan1_0    'Fan1�l�W���ߊJ�n�ʒu����(8/26����)
883     PScrewPosTemp(10) = PScrewFan1     'Fan1�l�W���ߏI���ʒu����(8/26����)
884     M_Out16(12672) = 7              '�l�W���߈ʒu�ԍ����M
885     MRtn = ScrewTight(PScrewPosTemp,2,6.7)       'Fan�l�W����1�̎��s(8/26����)
886     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
887     If MRtn = 1 Then GoTo *CompScrew7
888     Mov PInitialPosition
889     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
890     MScrewErrorCord% = MScrewErrorCord% + 7
891     fErrorProcess(11,MScrewErrorCord%,52,0)
892 '    fErrorProcess(11,59,52,0)
893     If M_20# = MNext% Then M_20# = MClear%
894     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
895     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
896     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
897     *CompScrew7
898     '
899     'FAN�p�l�W�����@�փl�W�����ɍs��
900     'GoSub *ScrewSupplyFan      '�ꎞ�R�����g�A�E�g(8/4����)
901     'ScrewPositionDebug_2()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
902     *RE_SCREW_GET_8                                '���g���C�p���x��
903     If M_20# = MContinue% Then M_20# = MClear%
904     ScrewGet(PGetScrewPosTemp , 11260 , 0)          '�l�W�󂯎��J�n
905     If M_20# = MClear% Then GoTo *Comp_Screw_8
906     If M_20# = MNext% Then M_20# = MClear%
907     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
908     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
909     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
910     *Comp_Screw_8
911     '�G�ԃl�W����
912 '    Mov PScrewFan2_1            '�G���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
913 '    Ovrd 5
914 '    Mvs PScrewFan2              '�G�l�W����
915 '    Ovrd 10
916 '    Mvs PScrewFan2_1            '�G���
917     PScrewPosTemp(1) = PScrewFan2_1    'Fan2�l�W���ߊJ�n�ʒu������(8/26����)
918     PScrewPosTemp(2) = PScrewFan2_0    'Fan2�l�W���ߊJ�n�ʒu����(8/26����)
919     PScrewPosTemp(10) = PScrewFan2     'Fan2�l�W���ߏI���ʒu����(8/26����)
920     M_Out16(12672) = 8              '�l�W���߈ʒu�ԍ����M
921     MRtn = ScrewTight(PScrewPosTemp,2,6.7)       'Fan�l�W����2�̎��s(8/26����)
922     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
923     If MRtn = 1 Then GoTo *CompScrew8
924     Mov PInitialPosition
925     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
926     MScrewErrorCord% = MScrewErrorCord% + 8
927     fErrorProcess(11,MScrewErrorCord%,52,0)
928 '    fErrorProcess(11,60,52,0)
929     If M_20# = MNext% Then M_20# = MClear%
930     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
931     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
932     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
933     *CompScrew8
934 '
935     '�v���O�������_
936     'Mov PInitialPosition        ' ���_���
937     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
938     Mov PTicketRead_1           ' �`�P�b�g���[�h�ʒu
939     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
940     InitialState()              ' ������Ԃɂ���
941     M_20# = MAssyOK%              ' ����I������
942     GoTo *fnAssyStart_FEndPosi
943 '
944 *ASSY_ERROR_END
945     fnInitialZone()   ' �����ʒu�Ɉړ�
946 *AssyEnd
947     InitialState()  ' ������Ԃɂ���
948 *fnAssyStart_FEndPosi
949     Exit Function
950 FEnd
951 '
952 '��fnPiasCheck
953 ''' <summary>
954 ''' PIAS�`�P�b�g�Ǎ���
955 ''' </summary>
956 ''' <returns>   0 : NG
957 '''             1 : OK(�Ǎ��݊���)
958 ''' </returns>
959 ''' <remarks>
960 ''' Date   : 2021/07/07 : M.Hayakawa
961 ''' </remarks>'
962 Function M% fnPiasCheck
963     fnPiasCheck = 0
964     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
965     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����
966 '
967 *RETRY_PIAS
968     M_20# = MClear%
969     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
970     '
971     '�yID�`�P�b�g�ǂݍ��݁z
972     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
973     MInspGroup%(1) = 1              '����G�ԍ�
974     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
975 '
976     '�G���[�̏ꍇ
977     If MRtn <> 1 Then
978         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
979         If MRtn <> 1 Then
980             'D720 -> D1300 �R�s�[�v��
981             M_Out(12565) = 1
982             Dly 0.5
983             M_Out(12565) = 0
984             '�G���[�����L�q
985             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
986             'GOT KEY���͑҂�
987             MKeyNumber = fnKEY_WAIT()
988             '
989             Select MKeyNumber
990                 Case MNext%         '���ւ�I�������ꍇ
991                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
992                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
993                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
994                     Break
995                 Case MAbout%        '��~��I�������ꍇ
996                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
997                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
998                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
999                     Break
1000                 Case MNgProcess%    'NG��I�������ꍇ
1001                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1002                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1003                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1004                     Break
1005                 Case MContinue%     '�p����I�������ꍇ
1006                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1007                     M_20# = MContinue%
1008                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1009                     Break
1010             End Select
1011         EndIf
1012     EndIf
1013     If M_20# = MPass% Then GoTo *fnPiasCheck_End            'PIAS�`�F�b�N�I��
1014     If M_20# = MAbout% Then GoTo *fnPiasCheck_End           'PIAS�`�F�b�N�I��
1015     If M_20# = MNgProcess% Then GoTo *fnPiasCheck_End       'PIAS�`�F�b�N�I��
1016     If M_20# = MContinue% Then GoTo *RETRY_PIAS             'PIAS�`�F�b�N���g���C
1017 '
1018 '----------D720 -> D1300 �R�s�[�v��----------
1019     M_Out(12565) = 1
1020     Dly 0.5
1021     M_Out(12565) = 0
1022 '----------�ʐM�m�F������----------
1023     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1024     MRtn = 0                ' ������
1025     M_20# = MClear%         ' ������
1026     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1027     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1028 '    If MRtn <> 1 Then
1029 '        If M_20# = MContinue% Then
1030 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1031 '        Else
1032 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1033 '        EndIf
1034 '    EndIf
1035     If MRtn = 1 Then GoTo *PCComu_OK                '�ʐMOK�����x���փW�����v
1036     If M_20# = MContinue% Then GoTo *RETRY_PIAS      '�`�P�b�g�ǂݒ������烊�g���C
1037     GoTo *fnPiasCheck_End                           '���̑��̏ꍇPIAS�`�F�b�N�I��
1038     *PCComu_OK
1039 '----------�H�������m�F----------
1040     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1041     MRtn = 0                ' ������
1042     M_20# = MClear%         ' ������
1043     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1044     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1045 '    If MRtn <> 1 Then
1046 '        If M_20# = MContinue% Then
1047 '            GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1048 '        Else
1049 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1050 '        EndIf
1051 '    EndIf
1052     If MRtn = 1 Then GoTo *ProcessCheck_OK                '�H���`�F�b�NOK�����x���փW�����v
1053     If M_20# = MContinue% Then GoTo *RETRY_PIAS      '�`�P�b�g�ǂݒ������烊�g���C
1054     GoTo *fnPiasCheck_End                           '���̑��̏ꍇPIAS�`�F�b�N�I��
1055     *ProcessCheck_OK
1056     '
1057     fnPiasCheck = 1
1058     *fnPiasCheck_End
1059     Exit Function
1060 FEnd
1061 '
1062 '��fnPCComuCheck
1063 ''' <summary>
1064 ''' PC-PLC�ʐM�`�F�b�N
1065 ''' </summary>
1066 ''' <returns>   0 : NG
1067 '''             1 : OK(�Ǎ��݊���)
1068 ''' </returns>
1069 ''' <remarks>
1070 ''' Date   : 2021/07/07 : M.Hayakawa
1071 ''' </remarks>'
1072 Function M% fnPCComuCheck
1073     fnPCComuCheck = 0
1074     MJudge% = 0                                  '������
1075     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1076     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1077     '
1078     For MStaNo = 0 To 5
1079         '
1080         If M_In(MIN_PIAS_ComOK%) = 1 Then
1081             'PC�ʐMOK(M400)
1082             MJudge% = MOK%
1083             MStaNo = 5
1084             Break
1085         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1086             'toRBT_�ʐM�m�Ftime out
1087             MJudge% = MNG%
1088             MCommentD1001 = 15
1089             MCommentD1002 = 21
1090             MStaNo = 5
1091             Break
1092         Else
1093             'toRBT_�ʐM�m�Ftime out
1094             MJudge% = MNG%
1095             MCommentD1001 = 14
1096             MCommentD1002 = 21
1097             Break
1098         EndIf
1099     Next MStaNo
1100     '
1101     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1102     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1103     '
1104     '�G���[���
1105     If MJudge% <> MOK% Then
1106         M_20# = MClear%     '������
1107         '�G���[�����L�q
1108         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1109         'GOT KEY���͑҂�
1110         MKeyNumber = fnKEY_WAIT()
1111         '
1112         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1113             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1114             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1115             Break
1116         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1117             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1118             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1119             Break
1120         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1121             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1122             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1123             Break
1124         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1125             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1126             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1127             Break
1128         EndIf
1129     Else
1130         'OK�̏ꍇ
1131         fnPCComuCheck = 1
1132     EndIf
1133     Exit Function
1134 FEnd
1135 '
1136 '��fnProcessCheck
1137 ''' <summary>
1138 ''' �H�������m�F
1139 ''' </summary>
1140 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1141 '''             -1�F�O�H������NG  -2�F���H����������
1142 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1143 '''             -5�F���������G���[
1144 ''' </returns>
1145 ''' <remarks>
1146 ''' Date   : 2021/07/07 : M.Hayakawa
1147 ''' </remarks>'
1148 Function M% fnProcessCheck
1149     fnProcessCheck = 0
1150     MJudge% = MNG%      '��UNG���������Ƃ���
1151 '----------�H�������m�F----------
1152     MCommentD1001 = 0   '�R�����g������
1153     For MStaNo = 0 To 5
1154         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1155         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1156         '
1157         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1158             MJudge% = MOK%
1159             fnAutoScreenComment(85)     ' AUTO���
1160             MStaNo = 5
1161             Break
1162         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1163             MFlgLoop% = 0
1164             MJudge% = MNG%
1165             MCommentD1001 = 27
1166             MCommentD1002 = 22
1167             fnAutoScreenComment(94)     ' AUTO���
1168             fnProcessCheck = -2         ' NG��-2��Ԃ�
1169             MStaNo = 5
1170             Break
1171         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1172            MJudge% = MNG%
1173             MCommentD1001 = 31
1174             MCommentD1002 = 22
1175             fnAutoScreenComment(83)     ' AUTO���
1176             fnProcessCheck = -3         ' NG��-3��Ԃ�
1177             MStaNo = 5
1178             Break
1179         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1180             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1181             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1182             MJudge% = MNG%
1183             MCommentD1001 = 32
1184             MCommentD1002 = 22
1185             fnAutoScreenComment(84)     ' AUTO���
1186             fnProcessCheck = -1         ' NG��-1��Ԃ�
1187             Dly 1.0
1188             '�H�������m�FOFF
1189             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1190             Dly 1.0
1191            'MStaNo = 5
1192             Break
1193         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1194             MFlgLoop% = 0
1195             MJudge% = MNG%
1196             MCommentD1001 = 29
1197             MCommentD1002 = 22
1198             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1199             fnProcessCheck = -5         ' NG��-5��Ԃ�
1200             MStaNo = 5
1201             Break
1202         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1203             MJudge% = MNG%
1204             If MCommentD1001 = 32 Then
1205                 '�������Ȃ�
1206             Else
1207                 MCommentD1001 = 26
1208             EndIf
1209             MCommentD1002 = 22
1210             fnProcessCheck = -4         ' NG��-4��Ԃ�
1211             MStaNo = 5
1212             Break
1213         Else
1214             MJudge% = MNG%
1215             MCommentD1001 = 28
1216             MCommentD1002 = 22
1217         EndIf
1218     Next MStaNo
1219     '�H�������m�FOFF
1220     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1221     '�ʉߗ���NG �H�������̏ꍇ
1222     If MJudge% = MPass% Then
1223         M_20# = MPass%
1224     EndIf
1225     '
1226     '�G���[���
1227     If MJudge% <> MOK% Then
1228         M_20# = MClear%     '������
1229         '�G���[�����L�q
1230         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1231         'GOT KEY���͑҂�
1232         MKeyNumber = fnKEY_WAIT()
1233         '
1234         Select MKeyNumber
1235             Case MAbout%        '��~��I�������ꍇ
1236                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1237                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1238                 Break
1239             Case MNext%         '���ւ�I�������ꍇ
1240                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1241                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1242                 Break
1243             Case MContinue%     '�p����I�������ꍇ
1244                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1245                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1246                 Break
1247             Case MNgProcess%    'NG��I�������ꍇ
1248                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1249                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1250                 Break
1251         End Select
1252     Else
1253         fnProcessCheck = 1  ' OK��1��Ԃ�
1254     EndIf
1255     Exit Function
1256 FEnd
1257 '
1258 '��fnPiasWrite
1259 ''' <summary>
1260 ''' Pias �g�����ʏ����ݗv��
1261 ''' </summary>
1262 '''<param name="MFlg%">
1263 ''' MOK%(1) = �H��������OK��������
1264 ''' MNG%(0) = �H��������NG��������
1265 '''</param>
1266 '''<returns></returns>
1267 ''' <remarks>
1268 ''' Date   : 2021/07/07 : M.Hayakawa
1269 ''' </remarks>'
1270 Function M% fnPiasWrite(ByVal MFlg%)
1271       fnPiasWrite = 0
1272 *RETRY_PIASWRITE
1273     '
1274     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1275    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1276     If MFlg% = MOK% Then
1277         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1278     Else
1279         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1280     EndIf
1281     Dly 0.1                  '�O�̂���
1282     '
1283     'Pias�֏����݊J�n M305 -> ON
1284     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1285     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1286     '
1287     MJudge% = MNG%
1288     '
1289     For MStaNo = 0 To 5
1290         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1291             MJudge% = MOK%
1292             'MRet = fnAutoScreenComment(85)  'AUTO���
1293             MStaNo = 5
1294             Break
1295         '
1296         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1297             MJudge% = MNG%
1298             'MRet = fnAutoScreenComment(85)  'AUTO���
1299            MCommentD1001 = 34
1300            MCommentD1002 = 25
1301             MStaNo = 5
1302             Break
1303         '
1304         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1305             MJudge% = MNG%
1306             'MRet = fnAutoScreenComment(85)  'AUTO���
1307            MCommentD1001 = 35
1308            MCommentD1002 = 25
1309             MStaNo = 5
1310             Break
1311         '
1312         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1313             MJudge% = MNG%
1314             'MRet = fnAutoScreenComment(85)  'AUTO���
1315            MCommentD1001 = 36
1316            MCommentD1002 = 25
1317             MStaNo = 5
1318             Break
1319         '
1320         Else
1321             MJudge% = MNG%
1322            MCommentD1001 = 42
1323            MCommentD1002 = 25
1324         '
1325         EndIf
1326         '
1327     Next MStaNo
1328     '
1329     'Pias�֏����݊J�n M305 -> OfF
1330     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1331     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1332     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1333     '
1334     '
1335     '�ʉߗ���NG �H�������̏ꍇ
1336     If MJudge% = MPass% Then
1337         M_20# = MPass%
1338     EndIf
1339     '
1340    M_20# = MClear%     '������
1341     '
1342     '�G���[���
1343     If MJudge% < MOK% Then
1344     '
1345 '�c���Ă���������ł͎g�p���Ȃ����x��
1346 *RETRY_ERR_WRITE
1347         M_20# = MClear%     '������
1348         '�G���[�����L�q
1349         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1350         'GOT KEY���͑҂�
1351         MKeyNumber = fnKEY_WAIT()
1352         '
1353         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1354             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1355             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1356             Break
1357         '
1358         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1359             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1360             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1361             Break
1362         '
1363         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1364             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1365             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1366             Break
1367         '
1368         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1369             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1370             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1371             Break
1372         '
1373         EndIf
1374         '
1375 '        If M_20# = MClear% Then *RETRY_ERR_WRITE
1376         '
1377     EndIf
1378     '
1379     If M_20# = MContinue% Then *RETRY_PIASWRITE
1380     '
1381     fnPiasWrite = 1
1382     Exit Function
1383 FEnd
1384 '
1385 '��fnPCBNumberCheck
1386 ''' <summary>
1387 ''' Pias ��ԍ��ƍ��v��
1388 ''' </summary>
1389 '''<returns>0�i�Œ�j</returns>
1390 ''' <remarks>
1391 ''' Date   : 2021/07/07 : M.Hayakawa
1392 ''' </remarks>'
1393 Function M% fnPCBNumberCheck
1394       fnPCBNumberCheck = 0
1395     '
1396 *RETRY_PCBCHECK
1397     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1398     'Pias�֊�ƍ��J�n M310 -> ON
1399     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1400     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1401     '
1402     MJudge% = MNG%
1403     '
1404     For MStaNo = 0 To 5
1405         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1406             MJudge% = MOK%
1407             fnAutoScreenComment(96)  'AUTO���
1408             MStaNo = 5
1409             Break
1410         '
1411         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1412             MJudge% = MNG%
1413             fnAutoScreenComment(97)  'AUTO���
1414             MCommentD1001 = 37
1415             MCommentD1002 = 25
1416             MStaNo = 5
1417             Break
1418         '
1419         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1420             MJudge% = MNG%
1421             fnAutoScreenComment(98)  'AUTO���
1422             MCommentD1001 = 38
1423             MCommentD1002 = 25
1424             MStaNo = 5
1425             Break
1426         '
1427         ElseIf M_In(11580) = 1 Then                         'time out
1428             MJudge% = MNG%
1429             fnAutoScreenComment(99)  'AUTO���
1430             MCommentD1001 = 39
1431             MCommentD1002 = 25
1432             MStaNo = 5
1433             Break
1434         '
1435         Else
1436             MJudge% = MNG%
1437            MCommentD1001 = 41
1438            MCommentD1002 = 25
1439         '
1440         EndIf
1441         '
1442     Next MStaNo
1443     '
1444     'Pias�֊�ƍ��J�n M310 -> OfF
1445     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1446     '
1447     '
1448     '�ʉߗ���NG �H�������̏ꍇ
1449     If MJudge% = MPass% Then
1450         M_20# = MPass%
1451     EndIf
1452     '
1453    M_20# = MClear%     '������
1454     '
1455     '�G���[���
1456     If MJudge% < MOK% Then
1457     '
1458 '�c���Ă���������ł͎g�p���Ȃ����x��
1459 *RETRY_ERR_PCBNUMBER
1460         M_20# = MClear%     '������
1461         '�G���[�����L�q
1462         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1463         'GOT KEY���͑҂�
1464         MKeyNumber = fnKEY_WAIT()
1465         '
1466         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1467             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1468             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1469             Break
1470         '
1471         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1472             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1473             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1474         '
1475         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1476             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1477             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1478         '
1479         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1480             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1481             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1482             Break
1483         '
1484         EndIf
1485         '
1486 '        If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1487         '
1488     EndIf
1489     '
1490     If M_20# = MContinue% Then *RETRY_PCBCHECK
1491     Exit Function
1492 FEnd
1493 '
1494 '��ScrewTight
1495 ''' <summary>
1496 ''' �˂����߂��s��(S�^�C�g)
1497 ''' </summary>
1498 '''<param name="PScrewPos()">
1499 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1500 '''             PScrewPos(2)    �F�˂����߉��_
1501 '''             PScrewPos(10)   �F�˂����ߏI������
1502 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
1503 '''             1:6mm S�^�C�g��l�W
1504 '''             2:8mm P�^�C�g
1505 '''             3:6mm S�^�C�g���l�W
1506 '''             4:13mm S�^�C�g
1507 '''             5:6mm M�l�W
1508 '''</param>
1509 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
1510 '''<returns>����
1511 '''         0=�ُ�I���A1=����I��
1512 '''</returns>
1513 ''' <remarks>
1514 ''' Date   : 2021/07/07 : M.Hayakawa
1515 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
1516 ''' </remarks>'
1517 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
1518     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1519     ScrewTight = 0
1520     MOKNGFlg = 0
1521     Ovrd 100
1522     Mov PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
1523     Fine 0.05 , P
1524     Ovrd MOvrdA%
1525     ' �����ݒ�
1526     Accel 100, 10
1527     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
1528     Mvs PScrewPosition(2)
1529     ' �����������ɖ߂�
1530     Accel
1531     ' ����Ovrd�ݒ�
1532 '    Ovrd MOvrdA%
1533     Ovrd 100
1534     ' Spd�ݒ�
1535 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1536     Spd MFeedSpd
1537     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
1538     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1539     Select MScrewType%
1540         Case 1
1541             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1542             ProgramBankSet(1,1)
1543             Break
1544         Case 2
1545             ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1546             ProgramBankSet(3,1)
1547             Break
1548         Case 3
1549             ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1550             ProgramBankSet(1,1)
1551             Break
1552         Case 4
1553             ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1554             ProgramBankSet(1,1)
1555             Break
1556         Case 5
1557             ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1558             ProgramBankSet(1,1)
1559             Break
1560         Case 6
1561             ' S�^�C�g�F�v���O����1�A�o���N4�ɐݒ�
1562             ProgramBankSet(1,4)
1563             Break
1564         Default
1565             ' �v���O����1�A�o���N�Ȃ��ݒ�
1566             ProgramBankSet(0,0)
1567             Break
1568     End Select
1569 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1570      '�h���C�o�[ON�@CW
1571     M_Out(12241)=1
1572     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1573     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
1574     Dly 0.1
1575     Fine 0 , P
1576     Spd M_NSpd
1577     '
1578     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
1579         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1580         Dly 0.1
1581        ' �v���O�����E�o���N����
1582         ProgramBankSet(0,0)
1583         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1584         Mvs PScrewPosition(10),-80
1585         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1586         M_Out(12249)=1 Dly 0.3
1587         MOKNGFlg = -1
1588         ScrewTight = 0
1589     Else
1590          '�h���C�o�[OFF�@CW
1591         M_Out(12241)=0
1592 '        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
1593 '        Select MScrewType%
1594 '            Case 1
1595 '                ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1596 '                ProgramBankSet(1,3)
1597 '                Break
1598 '            Case 2
1599 '                ' P�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1600 '                ProgramBankSet(3,3)
1601 '                Break
1602 '            Case 3
1603 '                ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
1604 '                ProgramBankSet(1,3)
1605 '                Break
1606 '            Case 4
1607 '                ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
1608 '                ProgramBankSet(1,3)
1609 '                Break
1610 '            Case 5
1611 '                ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
1612 '                ProgramBankSet(1,3)
1613 '                Break
1614 '            Default
1615 '                ' �v���O����1�A�o���N�Ȃ��ݒ�
1616 '                ProgramBankSet(0,0)
1617 '                Break
1618 '        End Select
1619 '         '�h���C�o�[ON�@CW
1620 '        Mvs PScrewPosition(10)
1621 '        M_Out(12241)=1
1622 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1623 '        Fine 0 , P
1624 '
1625          '�h���C�o�[OFF�@CW
1626         M_Out(12241)=0
1627        ' �v���O�����E�o���N����
1628         ProgramBankSet(0,0)
1629         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1630         M_Out(12249)=1 Dly 0.3
1631     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1632         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1633        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1634         'Mvs PScrewPosition(10),-80
1635         ScrewTight = 1
1636     EndIf
1637 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1638 '    Ovrd 10
1639 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1640     Ovrd 100
1641     Exit Function
1642 FEnd
1643 '
1644 '��ScrewGet
1645 ''' <summary>
1646 ''' �˂������@����˂��𓾂�
1647 ''' </summary>
1648 '''<param name="%">
1649 '''         PScrewPos(1)    �F�˂�������̂˂����
1650 '''         PScrewPos(2)    �F�˂���������_
1651 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1652 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1653 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1654 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1655 '''</param>
1656 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1657 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1658 '''<returns>����
1659 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1660 '''</returns>
1661 ''' <remarks>
1662 ''' Date   : 2021/07/07 : M.Hayakawa
1663 ''' </remarks>
1664 '''<update>
1665 '''Date    : 2021/11/15 : ����
1666 '''Date    : 2021/02/07 : ���� �O�̂��ߊm�F���폜
1667 '''</update>
1668 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1669     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
1670     ScrewGet = 0
1671     MScrewJudge% = 0
1672     '�˂������평������G���[�`�F�b�N
1673 ' ���b��폜
1674     'Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
1675     For MCnt% = 0 To MFinCnt%
1676         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1677         If MRtn = 0 Then
1678             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1679             ScrewGet = -1
1680             MScrewJudge% = 2
1681         EndIf
1682         Ovrd 100
1683         If FeederScrewSensor% <> 0 Then
1684             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1685                 'Ovrd 30
1686                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1687                 'NG�Ƃ��Ă����̊֐����甲����
1688                 ScrewGet = -2
1689                 MScrewJudge% = 3
1690             EndIf
1691         EndIf
1692         Ovrd 100
1693         Spd M_NSpd
1694         If MScrewJudge% = 0 Then
1695     '        ScrewGet = 0
1696             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1697             Dly 0.3
1698             MScrewCnt% = 0
1699             MFinCnt% = 2
1700             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1701             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1702             '
1703             '
1704             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1705             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1706             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1707             'Mvs PScrewPosition(10), 1.2
1708            Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
1709             '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX3/30����)
1710             M_Out(Y60_Driver)=1
1711             '�r�b�g��]�����ԊĎ��J�n
1712             M_Timer(4) = 0
1713             MloopFlg = 0
1714             MCrtTime& = 0
1715            '�r�b�g��]����܂őҋ@
1716             While MloopFlg = 0
1717                 MCrtTime& = M_Timer(4)
1718                 If MCrtTime& >= 180 Then
1719                     MloopFlg = 1
1720                 EndIf
1721             WEnd
1722             '
1723            M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1724             '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX3/30����)
1725 '            M_Out(Y60_Driver)=1
1726 '            Dly 0.2
1727             '�z���ʒu�ɂċz���m�F
1728             MRtn = 0
1729             MRtn = frInCheck(11264, 1, MSETTIMEOUT01&)      '�`�F�b�N�͂��邪�G���[����͂��Ȃ�
1730             '
1731             JOvrd M_NJovrd
1732             Spd M_NSpd
1733             '�l�W�z���m�F�ʒu�ړ�
1734             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1735             Mvs PScrewPosition(10), -30  ' �l�W�z���m�F�ʒu
1736            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1737             '�r�b�g��]��~
1738             M_Out(Y60_Driver)=0
1739             '
1740 '            If MRtn = 1 Then       '�ŏ���臒l�`�F�b�N���s��Ȃ�
1741                 '1�b�ԃl�W�z���m�F �n�߂�臒l
1742                 MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1743 '            EndIf
1744             'MRtn = 0'�����G���[
1745             '�z���G���[�̏ꍇ
1746             '�l�W���˂����Y�ɖ߂�
1747             If MRtn = 0 Then
1748                 Ovrd 30      '2����5�ɕύX'5����30�ɕύX(3/30����)
1749                 '�r�b�g��]��~
1750                 M_Out(Y60_Driver)=0
1751                 '�l�W�����@���
1752                 Mvs PScrewPosition(1)
1753                 '�X�ɏ��
1754                 Mov PScrewPosition(1), -140
1755                 '�l�W�̂Ĉʒu
1756                 If FeederReadyNo% = 11260 Then     '�����@�ʂɋz���G���[�����J�E���g�@2022/05/19 �n��
1757                     MRtn = FnCtlValue2(3)          '�����@�Q�z���G���[���{�P
1758                 Else
1759                     MRtn = FnCtlValue2(4)          '�����@�P�z���G���[���{�P  2022/04/28 �n��
1760                 EndIf
1761                 Mov PScrewPosition(9)
1762                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1763                 '�z��OFF
1764                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1765                 Dly 0.2
1766                 '�j��ON
1767                 M_Out(Y6B_VB1)=1 '�^��j��ON
1768                 '�r�b�g��]
1769                 M_Out(Y61_Driver)=1
1770                 Dly 0.5
1771                 '                '
1772                 Ovrd 100
1773                 JOvrd M_NJovrd
1774                 Spd M_NSpd
1775                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1776                 Mov PScrewPosition(9), 10
1777                 Mov PScrewPosition(9)
1778                 Dly 0.1
1779                 Mov PScrewPosition(9), 10
1780                 Mov PScrewPosition(9)
1781                 '
1782                 '�l�W�����҂�
1783                 Wait M_In(11265) = 0
1784                 '�r�b�g��]��~
1785                 M_Out(Y61_Driver)=0
1786                 Dly 0.1
1787                 '�j��OFF
1788                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1789                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1790                 Mov PScrewPosition(1), -140
1791                 Ovrd 100
1792                 Spd M_NSpd
1793                 '�l�W�����@���
1794                 Mvs PScrewPosition(1)
1795 '                '
1796                 ScrewGet = -3
1797                 If MCnt% = MFinCnt% Then
1798                     MScrewJudge% = 4
1799                     Mov PScrewPosition(2)
1800                     Break
1801                 EndIf
1802                 Break
1803 '                '
1804             Else
1805                 MCnt% = MFinCnt%
1806                 ScrewGet = 1
1807             EndIf
1808         Else
1809             MCnt% =MFinCnt%
1810         EndIf
1811     Next  MCnt%
1812         '
1813 '    If MScrewJudge% = 0 Then
1814 '        Ovrd 100
1815 '        Spd M_NSpd
1816 '        PScrewPosition(1)
1817 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1818 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1819 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1820 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1821 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1822 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1823 '        'Mov PScrewPosition(2)
1824 '        '������x�z���m�F�@���̍ŏI臒l
1825 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1826 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1827 '            MScrewJudge% = 4
1828 '            ScrewGet = -3
1829 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1830 '            MScrewJudge% = 1
1831 '            ScrewGet = 1
1832 '        EndIf
1833 '        Break
1834 '    EndIf
1835     '
1836 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1837     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1838     '
1839     Select MScrewJudge%
1840 '        Case 0
1841 ''            fErrorProcess(11,162,163,0) '�ُ�I��
1842 '            MCommentD1001 = 162
1843 '            MCommentD1002 = 96
1844 '            Break
1845         Case 2
1846 '            fErrorProcess(11,63,161,0) '����NG
1847             MCommentD1001 = 63
1848             MCommentD1002 = 96
1849             Break
1850         Case 3
1851 '            fErrorProcess(11,160,164,0) '�닟��
1852             MCommentD1001 = 237
1853             MCommentD1002 = 96
1854             Break
1855         Case 4
1856 '            fErrorProcess(11,94,95,0) '�z��NG
1857             MCommentD1001 = 94
1858             MCommentD1002 = 95
1859             Break
1860     End Select
1861     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1862     '
1863     Select M_20#
1864         Case MAbout%          '��~�������ꂽ�ꍇ
1865             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
1866             Mov PInitialPosition
1867             Break
1868         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1869             Break
1870         Case MNext%           '�p���������ꂽ�ꍇ
1871             M_20# = MClear%     '������
1872             Break
1873         Case MNgProcess%      'NG�������ꂽ�ꍇ
1874             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
1875             Mov PInitialPosition
1876             Break
1877         End Select
1878 *End_ScrewGet
1879     Exit Function
1880 FEnd
1881 '
1882 '��ProgramBankSet
1883 ''' <summary>
1884 ''' �˂����߂��s��(P�^�C�g)
1885 ''' </summary>
1886 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1887 '''<param name="MBankNo">�o���N�ԍ�</param>
1888 '''</returns>
1889 ''' <remarks>
1890 ''' Date   : 2021/10/05 : M.Hayakawa
1891 ''' </remarks>'
1892 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1893 '
1894     MLocalPrgNo% = (MProgramNo% - 1) * 32
1895     MLocalBankNo% = MBankNo% * 4
1896 '
1897     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1898         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1899     Else
1900         MLocalOutNo% = 0
1901     EndIf
1902 '
1903     M_Out8(12240) = MLocalOutNo%
1904     Dly 0.1
1905     Exit Function
1906 FEnd
1907 '
1908 '��fnKEY_WAIT()
1909 ''' <summary>
1910 ''' GOT����̃L�[���͑҂�
1911 ''' </summary>
1912 '''<returns>1�F��~    2�F����
1913 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1914 '''         5�FNG
1915 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1916 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1917 '''</returns>
1918 ''' <remarks>
1919 ''' Date   : 2021/07/07 : M.Hayakawa
1920 ''' </remarks>'
1921 Function M% fnKEY_WAIT()
1922     fnKEY_WAIT = 0
1923     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1924     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1925     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1926     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1927     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1928     Dly 0.2
1929     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1930     MLocalLoopFlg=1
1931     While MLocalLoopFlg=1
1932         If M_In(11345) = 1 Then         '��~   M5345
1933             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1934             fnKEY_WAIT = 1
1935             MLocalLoopFlg=-1
1936             Break
1937         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1938             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1939             fnKEY_WAIT = 2
1940             MLocalLoopFlg=-1
1941             Break
1942         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1943             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1944             fnKEY_WAIT = 3
1945             MLocalLoopFlg=-1
1946             Break
1947         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1948             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1949             fnKEY_WAIT = 4
1950             MLocalLoopFlg=-1
1951             Break
1952         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1953             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1954             fnKEY_WAIT = 5
1955             MLocalLoopFlg=-1
1956             Break
1957         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1958             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1959             fnKEY_WAIT = MRobotInit1%
1960             MLocalLoopFlg=-1
1961             Break
1962         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1963             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1964             fnKEY_WAIT = MRobotInit2%
1965             MLocalLoopFlg=-1
1966             Break
1967         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1968             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1969             fnKEY_WAIT = MRobotInit3%
1970             MLocalLoopFlg=-1
1971             Break
1972         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1973             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1974             fnKEY_WAIT = MRobotInit4%
1975             MLocalLoopFlg=-1
1976             Break
1977         Else
1978         EndIf
1979     WEnd
1980     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1981     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
1982     Exit Function
1983 FEnd
1984 '
1985 '�� fnAUTO_CTL
1986 ''' <summary>
1987 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
1988 ''' </summary>
1989 ''' <remarks>
1990 ''' Date   : 2021/07/07 : M.Hayakawa
1991 ''' </remarks>
1992 Function M% fnAUTO_CTL
1993     fnAUTO_CTL = 0
1994     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1995     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
1996     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1997     '
1998     If M_Svo=0 Then             '�T�[�{ON�m�F
1999         Servo On
2000     EndIf
2001     Wait M_Svo=1
2002     Exit Function
2003 FEnd
2004 '
2005 '�� fnWindScreenOpen
2006 ''' <summary>
2007 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2008 ''' </summary>
2009 '''<param name="%"></param>
2010 '''<param name="%"></param>
2011 '''<param name="%"></param>
2012 '''<param name="%"></param>
2013 ''' <remarks>
2014 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2015 ''' MWindReSet = 0     ��ʔ�\��
2016 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2017 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2018 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2019 ''' Date   : 2021/07/07 : M.Hayakawa
2020 ''' </remarks>
2021 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2022     If MCommentD1001 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2023         M_Out16(12480) = MCommentD1001      'D1001 �R�����g
2024     EndIf
2025     '
2026     If MCommentD1002 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2027         M_Out16(12496) = MCommentD1002      'D1002 �R�����g
2028     EndIf
2029     '
2030     If MCommentD1003 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2031        M_Out16(12512) = MCommentD1003       'D1003 �R�����g
2032     EndIf
2033     '
2034     M_Out16(12448) = MScreenNo              '��ʔԍ�  M6448   10=�G���[���
2035     M_Out(12363) = 1 Dly 0.5                '�E�B���h��ʐݒ�  M6362
2036     Exit Function
2037 FEnd
2038 '
2039 '��FnCtlValue2
2040 ''' <summary>
2041 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2042 ''' </summary>
2043 ''' <param name="MCtlNo%"></param>
2044 ''' <remarks>
2045 ''' Date : 2022/04/28 �n��
2046 ''' </remarks>
2047 '''
2048 '''  1�F������       �{�P
2049 '''  2�F�g���n�j��   �{�P
2050 '''  3�F�����@�Q�z���G���[�� �{�P�@�@�g��NG����ύX 2022/05/19 �n��
2051 '''  4�F�����@�P�z���G���[�� �{�P
2052 ''' 99�F�Ǐ��J�n�M�� OFF
2053 '''
2054 Function M% FnCtlValue2(ByVal MCtlNo%)
2055     FnCtlValue2 = 1
2056     Select MCtlNo%
2057         Case 1        '�������{�P
2058             M_Out(12569) = 0             '�����݊J�n�M��OFF
2059             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2060             MInputQty = M_In16(11600)    '��������M
2061             MInputQty = MInputQty + 1    '�������{�P
2062             M_Out16(12592) = MInputQty   '���������M
2063             M_Out(12569) = 1             '�����݊J�n�M��ON
2064             Break
2065             '
2066         Case 2        '�g���n�j���{�P
2067             M_Out(12569) = 0             '�����݊J�n�M��OFF
2068             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2069             MAssyOkQty = M_In16(11616)   '�g��OK����M
2070             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2071             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2072             M_Out(12569) = 1             '�����݊J�n�M��ON
2073             Break
2074             '
2075         Case 3        '�����@�Q�z���G���[���{�P
2076             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2077             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2078             MSuctionErrQty = M_In16(11632)         '�����@�Q�z���G���[����M
2079             MSuctionErrQty = MSuctionErrQty + 1    '�����@�Q�z���G���[���{�P
2080             M_Out16(12624) = MSuctionErrQty        '�����@�Q�z���G���[�����M
2081             M_Out(12569) = 1                       '�����݊J�n�M��ON
2082             Break
2083             '
2084         Case 4        '�����@�P�z���G���[���{�P
2085             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2086             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2087             MSuctionErrQty = M_In16(11648)         '�����@�P�z���G���[����M
2088             MSuctionErrQty = MSuctionErrQty + 1    '�����@�P�z���G���[���{�P
2089             M_Out16(12640) = MSuctionErrQty        '�����@�P�z���G���[�����M
2090             M_Out(12569) = 1                       '�����݊J�n�M��ON
2091             Break
2092             '
2093         Case 99        '�Ǐ��J�n�M��OFF
2094             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2095             M_Out(12569) = 0        '�����݊J�n�M��OFF
2096             Break
2097             '
2098     End Select
2099     Exit Function
2100 FEnd
2101 '
2102 '
2103 '��FnScreEroorCord
2104 ''' �d���h���C�o�[�̃G���[�R�[�h���܂߂��R�����g���o���ׂ̃R�����g�ԍ��̍쐬
2105 ''' �V�K�쐬�F2022/05/23 : �n��
2106 '''
2107 Function M% FnScreEroorCord()
2108     MScrewErrorCord% = 0
2109     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2110     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2111     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2112     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2113     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2114     MScrewErrorCord% = MScrewErrorCord% * 10
2115     MScrewErrorCord% = MScrewErrorCord% + 500
2116     FnScreEroorCord = MScrewErrorCord%
2117     Exit Function
2118 FEnd
2119 '
2120 '
2121 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2122 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2123 '-------------------------------------------------------------------------------
2124 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2125 '   ����
2126 '       PInspPos()      �F�����ʒu
2127 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2128 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2129 '       MInspCnt%       �F�����ʒu��
2130 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2131 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2132 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2133 '   �߂�l�F����
2134 '       0=�ُ�I���A1=����I��
2135 '
2136 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2137 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2138 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2139 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2140 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2141 '-------------------------------------------------------------------------------
2142     '----- �����ݒ� -----
2143     Cnt 0                                                           '�ړ�����������(�����l=0)
2144     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2145 '    Cnt 1,0.1,0.1
2146     '�ϐ��錾�E������
2147     Def Inte MNum                                                   '�����ԍ�(������1�`)
2148     MNum% = 1                                                       '�����ԍ������l�ݒ�
2149     Def Inte MEndFlg                                                '�����I���t���O
2150     MEndFlg% = 0
2151     '
2152     '����G�ԍ��ݒ�v���E�������s�v��off
2153     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2154     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2155     '�G���[�ԍ��N���A
2156     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2157     M_Out16(MOUT_InspErrNum) = MInspErrNum
2158     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2159     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2160     '
2161     'Insight Ready check?
2162     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2163         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2164         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2165         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2166         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2167         'Exit Function
2168     EndIf
2169     If MInspErrNum = 20 Then GoTo *ISInspectionSingle_End
2170     '
2171     '�����ʒu���m�F
2172     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2173         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2174         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2175         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2176         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2177         'Exit Function
2178     EndIf
2179    If MInspErrNum = 21 Then GoTo *ISInspectionSingle_End
2180     '
2181     '
2182     '
2183     '----- ���C������ -----
2184     '�ݒ肳�ꂽ�����ʒu�����̌������s
2185     While( MEndFlg% = 0 )
2186         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2187         MSetGrNumRetryExitFlg = 0
2188         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2189         While( MSetGrNumRetryExitFlg = 0 )
2190         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2191             '
2192             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2193             '
2194             '----- �����O���[�v�ԍ��ݒ� -----
2195             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2196             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2197             '
2198             '�����ʒu�ֈړ��E�ړ������҂�
2199             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2200             Mov PInspPos( MNum% )                                       '�ړ�
2201             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2202             Dly 0.2                                                     '�ړ�������Delay 0.05>>0.2
2203             '
2204             '�����O���[�v�ԍ��ݒ�I���m�F
2205             M_Timer(1) = 0
2206             MExitFlg = 0
2207             While( MExitFlg = 0 )
2208                 '����G�ݒ萳��I��?
2209                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2210                     MExitFlg = 1
2211                 '
2212                 '����G�ݒ�ُ�I��?
2213                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2214                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2215                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2216                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2217                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2218                     EndIf
2219                     MExitFlg = 1
2220                 '
2221                 'timeout�`�F�b�N
2222                 ElseIf 1000 < M_Timer(1) Then
2223                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2224                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2225                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2226                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2227                     EndIf
2228                     MExitFlg = 1
2229                 EndIf
2230             WEnd
2231             '
2232             '����G�ԍ��ݒ�v��off
2233             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2234             '
2235             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2236             'NG�Ȃ���Δ�����
2237             If MCurrentStepErr = 0 Then
2238                 MSetGrNumRetryExitFlg = 1
2239             Else
2240                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2241                 If MSetGrNumRetryCnt = 0 Then
2242                     MSetGrNumRetryExitFlg = 1
2243                 Else
2244                     'Retry�ց@���̑O��Delay
2245                     Dly 0.5
2246                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2247                 EndIf
2248             EndIf
2249             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2250             '
2251         WEnd
2252         '
2253         '
2254         '
2255         '----- �������s -----
2256         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2257             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2258                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2259                 MInspRetryExitFlg = 0
2260                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2261                 While( MInspRetryExitFlg = 0 )
2262                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2263                     '
2264                     '���������m�F
2265                     MRetryCnt = MRetryCnt - 1
2266                     M_Timer(1) = 0
2267                     MExitFlg = 0
2268                     While( MExitFlg = 0 )
2269                     '���������҂�
2270                         '����OK�I��?
2271                         If M_In( MIN_IS_InspOK% ) = 1  Then
2272                             MJudgeOKFlg = 1                         '����OK�t���OON
2273                             MExitFlg = 1
2274                         '
2275                         '����NG�I��?
2276                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2277                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2278                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2279                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2280                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2281                                 EndIf
2282                             EndIf
2283                             MExitFlg = 1
2284                         '
2285                         '�����ُ�I��(IS timeout)?
2286                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2287                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2288                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2289                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2290                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2291                                 EndIf
2292                             EndIf
2293                             MExitFlg = 1
2294                         '
2295                         'timeout�`�F�b�N
2296                         ElseIf 3000 < M_Timer(1) Then
2297                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2298                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2299                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2300                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2301                                 EndIf
2302                             EndIf
2303                             MExitFlg = 1
2304                         EndIf
2305                     WEnd
2306                     '
2307                     '�����J�n�v��off
2308                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2309                     '
2310                     'OK�Ȃ甲����
2311                     If MJudgeOKFlg = 1 Then
2312                         MInspRetryExitFlg = 1
2313                     Else
2314                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2315                         If MRetryCnt = 0 Then
2316                             MInspRetryExitFlg = 1
2317                         Else
2318                             'Retry�ց@���̑O��Delay
2319                             Dly 0.3
2320                         EndIf
2321                     EndIf
2322                     '
2323                 WEnd
2324             EndIf
2325         EndIf
2326         '
2327         '
2328         '
2329         MNum% = MNum% + 1                                           '����Step+1
2330         '�����I���m�F�@�����I���t���O�Z�b�g
2331         If (MInspCnt% < MNum% ) Then
2332             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2333         EndIf
2334         'NG���������s������
2335         If MInspErrNum <> 0 Then                                    'NG����?
2336             If MNgContinue% <> 1 Then                               'NG���s?
2337                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2338             EndIf
2339         EndIf
2340     WEnd
2341     '
2342     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2343     If 0 < MZAxis% Then
2344         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2345         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2346         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2347         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2348     EndIf
2349     '
2350     '�߂�l�ݒ�
2351     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2352         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2353     Else
2354         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2355         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2356         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2357     EndIf
2358 '
2359 *ISInspectionSingle_End
2360     Exit Function
2361 FEnd
2362 '
2363 '��fnAutoScreenComment
2364 ''' <summary>
2365 ''' ���C����ʂ̓���󋵕\��
2366 ''' �R�����gD1005�̐ݒ�
2367 ''' </summary>
2368 '''<param name="McommentD1005%">�R�����gID</param>
2369 ''' <remarks>
2370 ''' Date   : 2021/07/07 : M.Hayakawa
2371 ''' </remarks>
2372 Function fnAutoScreenComment(ByVal McommentD1005%)
2373     M_Out16(12576) = McommentD1005%
2374     Exit Function
2375 FEnd
2376 '
2377 '��fnRoboPosChk
2378 ''' <summary>
2379 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2380 ''' </summary>
2381 '''<param name="MINNumber%">���͔ԍ�</param>
2382 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2383 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2384 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2385 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2386 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2387 ''' <remarks>
2388 ''' Date   : 2021/07/07 : M.Hayakawa
2389 ''' </remarks>
2390 Function M% fnRoboPosChk
2391     fnRoboPosChk = 0
2392     MRet = fnStepRead()
2393     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2394     '�E�B���h��ʐ؊���
2395     If MRBTOpeGroupNo > 5 Then
2396         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2397         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2398         Dly 0.2
2399         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2400         Dly 1.5
2401         '
2402         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2403         '
2404         MLoopFlg% = 1
2405         While MLoopFlg% = 1
2406             '
2407             '
2408             MKeyNumber% = fnKEY_WAIT()
2409             Select MKeyNumber%
2410                 Case Is = MAbout%       '��~
2411                     M_20# = MAbout%
2412                     MLoopFlg% = -1
2413                     Break
2414                 Case Is = MNext%        '����
2415                     'MLoopFlg% = -1
2416                     Break
2417                 Case Is = MContinue%    '�p��
2418                     M_20# = MContinue%
2419                     MLoopFlg% = -1
2420                     Break
2421                 Default
2422                     Break
2423             End Select
2424         WEnd
2425     EndIf
2426     '
2427     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2428         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2429         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2430         Select MRBTOpeGroupNo
2431             Case Is = 5                          '�������Ȃ�
2432                 Break
2433             Case Is = 10                         '�����ʒu�֖߂�
2434                 'Mov PTEST001
2435                 Break
2436             Case Is = 15                         '�����ʒu�֖߂�
2437                 'Mov PTEST002
2438                 Dly 0.5
2439                 'Mov PTEST001
2440                 Dly 0.5
2441                 Break
2442             Default
2443                 Break
2444         End Select
2445         '
2446         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2447         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2448         MRBTOpeGroupNo = 5
2449         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2450         Dly 1.0
2451         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2452         fnRoboPosChk = 1                        '�����ʒu������s
2453         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2454     EndIf
2455     Exit Function
2456 FEnd
2457 '
2458 '��frInCheck
2459 ''' <summary>
2460 ''' �Z���T�[IN�`�F�b�N
2461 ''' </summary>
2462 '''<param name="MINNumber%">���͔ԍ�</param>
2463 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2464 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2465 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2466 ''' <remarks>
2467 ''' Date   : 2021/07/07 : M.Hayakawa
2468 ''' </remarks>
2469 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2470     M_Timer(4) = 0
2471     MloopFlg = 0
2472     While MloopFlg = 0
2473         MCrtTime& = M_Timer(4)
2474         If M_In(MINNumber%) = MCMPFLG% Then
2475             MloopFlg = 1
2476             frInCheck = 1
2477         ElseIf MCrtTime& > MTimeCnt& Then
2478             MloopFlg = 1
2479             frInCheck = 0
2480         EndIf
2481     WEnd
2482     Exit Function
2483 FEnd
2484 '-----------------------------------------------
2485 '
2486 '�˂����ߋ@�ʐM�m�F
2487 '
2488 '-----------------------------------------------
2489 Function M% fScewTcomChk
2490     fScewTcomChk = 0
2491     '�ʐM�m�F���M
2492     M_Out(MOUT_ScwT_ComChk%) = MOn%
2493     '�ʐM�m�F��M�ҋ@
2494     Wait M_In(MIN_ScwT_comOK%) = MOn%
2495     '�ʐM�m�F���M�I��
2496     M_Out(MOUT_ScwT_ComChk%) = MOff%
2497     Exit Function
2498 FEnd
2499 '
2500 '
2501 '-----------------------------------------------
2502 '
2503 '�˂����ߊJ�n���M
2504 '
2505 '-----------------------------------------------
2506 Function M% fScewTStart
2507     fScewTStart = 0
2508     '�˂����ߊJ�n�ҋ@����M
2509     Wait M_In(MIN_ScwT_STRec%) = MOn%
2510     Dly 0.1
2511     '�˂����ߊJ�n��M�𑗐M
2512     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
2513     Exit Function
2514 FEnd
2515 '
2516 '
2517 '-----------------------------------------------
2518 '
2519 '�˂����ߊ�����M
2520 '
2521 '-----------------------------------------------
2522 Function M% fScewTFinish
2523     fScewTFinish = 0
2524     '�˂����ߊ����ҋ@����M
2525     Wait M_In(MIN_ScwT_Fin%) = MOn%
2526     Dly 0.1
2527     '�˂����ߊ�����M�𑗐M
2528     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
2529     Exit Function
2530 FEnd
2531 '
2532 '
2533 '-----------------------------------------------
2534 '
2535 '����xx��~��M
2536 '
2537 '-----------------------------------------------
2538 Function M% fScewTCaseStop(ByVal MCase%())
2539     fScewTCaseStop = 0
2540     '����xx��~����M
2541     Wait M_In(MCase%(1)) = MOn%
2542     Dly 0.1
2543     '����xx��~��M�𑗐M
2544     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
2545     Exit Function
2546 FEnd
2547 '
2548 '-----------------------------------------------
2549 '
2550 '�ĊJ�n��M
2551 '
2552 '-----------------------------------------------
2553 Function M% fScewTReStart()
2554     fScewTReStart = 0
2555     '�ĊJ�n����M
2556     Wait M_In(MIN_ScwT_ReST%) = MOn%
2557     Dly 0.1
2558     '�ĊJ�n��M�𑗐M
2559     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
2560     Exit Function
2561 FEnd
2562 '
2563 '��fErrorProcess
2564 '<summary>
2565 '�G���[����
2566 '</summary>
2567 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2568 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2569 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2570 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2571 '<make>
2572 '2021/11/5 �����V��
2573 '</make>
2574 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2575     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2576     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2577     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2578     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2579 *RETRY_ERR_PROCESS
2580      M_20# = MClear%     '������
2581 '        '�G���[�����L�q
2582         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2583 '        'GOT KEY���͑҂�
2584         MKeyNumber = fnKEY_WAIT()
2585 '        '
2586         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2587             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2588  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2589             Break
2590          '
2591         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2592             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2593  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2594         '
2595         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2596             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2597  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2598          '
2599         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2600             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2601  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2602             Break
2603         '
2604         EndIf
2605         '
2606         '
2607         '
2608         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2609         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2610         Exit Function
2611 FEnd
2612 '
2613 '��fnInitialZone
2614 ''' <summary>
2615 ''' ���݈ʒu������ɑҔ����A�����ʒu�ɖ߂�
2616 ''' </summary>
2617 ''' <remarks>
2618 ''' Date : 2021/12/2 : M.Hayakawa
2619 ''' Update:2022/06/2 : M.Hayakawa ���H���̔���~���A�ɍ��킹�ĕύX
2620 ''' </remarks>
2621 Function fnInitialZone()
2622     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���]
2623 '
2624     Ovrd 5
2625 ' ���ޔ�
2626     PActive = P_Curr
2627     Pmove = PActive
2628 '
2629     If PActive.X > 580 Then
2630         Pmove.Z =380        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2631     Else
2632         Pmove.Z =500        '��L�ȊO��Z:500�܂Ŏ����グ
2633     EndIf
2634 '
2635     Mvs Pmove
2636     Mov PInitialPosition
2637 ' ���b�N���J��
2638     InitialState()
2639 ' ��U��~
2640     fErrorProcess(20,70,256,0)
2641     Exit Function
2642 FEnd
2643 '
2644 '��InitialState
2645 ''' <summary>
2646 ''' �n���h�A����������ʒu�ɂ���
2647 ''' </summary>
2648 ''' <returns>   0 : OK
2649 '''             1 : NG
2650 ''' </returns>
2651 ''' <remarks>
2652 ''' Date : 2021/12/2 : M.Hayakawa
2653 ''' </remarks>
2654 Function M% InitialState()
2655     InitialState = 0
2656     '���i�ʒu���߉���
2657     M_Out(12261)=1 Dly 0.3      'FAN�N�����v�o�[�p���X�o��
2658     'Wait M_In(11271)=1          'FAN�N�����v�o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2659     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'FAN�N�����v�o�[���o(8/26����)
2660     If MRtn = 0 Then
2661         fErrorProcess(11,234,284,0)
2662         Select M_20#
2663             Case MAbout%            '��~��
2664                 InitialState = 1
2665                 Break
2666             Case MNgProcess%        'NG�������ꂽ�ꍇ
2667                 InitialState = 0
2668                 Break
2669             Case MContinue%
2670                 M_20# = MClear%
2671                 InitialState = 0
2672                 Break
2673             Case MNext%
2674                 M_20# = MClear%
2675                 InitialState = 0
2676                 Break
2677         End Select
2678     EndIf
2679     '
2680     M_Out(12259)=1 Dly 0.3      '�v�b�V��CY�pSV�ߒ[�p���X�o��
2681     'Wait M_In(11269)=1          '�v�b�V���ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2682     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    '�v�b�V���ߒ[���o
2683     If MRtn = 0 Then
2684         fErrorProcess(11,234,284,0)
2685         Select M_20#
2686             Case MAbout%            '��~��
2687                 InitialState = 1
2688                 Break
2689             Case MNgProcess%        'NG�������ꂽ�ꍇ
2690                 InitialState = 1
2691                 Break
2692             Case MContinue%
2693                 M_20# = MClear%
2694                 InitialState = 0
2695                 Break
2696             Case MNext%
2697                 M_20# = MClear%
2698                 InitialState = 0
2699                 Break
2700         End Select
2701     EndIf
2702     '
2703     M_Out(12257)=1 Dly 0.3      '�ʒu����CY�pSV�ߒ[�p���X�o��
2704     'Wait M_In(11267)=1          '�ʒu���ߖߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2705     MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[���o(8/26����)
2706     If MRtn = 0 Then
2707         fErrorProcess(11,234,284,0)
2708         Select M_20#
2709             Case MAbout%            '��~��
2710                 InitialState = 1
2711                 Break
2712             Case MNgProcess%        'NG�������ꂽ�ꍇ
2713                 InitialState = 1
2714                 Break
2715             Case MContinue%
2716                 M_20# = MClear%
2717                 InitialState = 0
2718                 Break
2719             Case MNext%
2720                 M_20# = MClear%
2721                 InitialState = 0
2722                 Break
2723         End Select
2724     EndIf
2725     Exit Function
2726 FEnd
2727 '
2728 '��fnTorqueCheck
2729 ''' <summary>
2730 ''' �g���N�`�F�b�N����p�̃��C��
2731 ''' </summary>
2732 ''' <remarks>
2733 ''' Date   : 2021/12/21 : H.AJI
2734 ''' </remarks>'
2735 Function M% fnTorqueCheck
2736     '�g���N�`�F�b�N�����M  �����n��~
2737     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2738     '
2739     fnTorqueCheck = 0
2740     Ovrd 20
2741     Mov PInitialPosition              '�����ʒu�ړ�
2742     Ovrd 100
2743     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2744     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2745     Dly 0.2
2746     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2747     '
2748     'M6340  �g���N�`�F�b�N��M
2749     M_Out(12340) = 1 Dly 1.0          '�g���N�`�F�b�N��M M6340
2750     '
2751     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
2752     '
2753     MLoopFlg = 1
2754     While MLoopFlg = 1
2755         '
2756         Mov PInitialPosition              '�����ʒu�ړ�
2757         '
2758         MKeyNumber = fnKEY_WAIT()
2759         Select MKeyNumber
2760             Case Is = 1           '��~
2761                 M_Out(12343) = 1 Dly 1.0         '��~�v���J�n�v����M M6343
2762                 Ovrd 20
2763                 Mov PTicketRead_1
2764                 Ovrd 100
2765                 M_20# = 1
2766                 MLoopFlg = -1
2767                 Break
2768             Case Is = 2           '����
2769                 Break
2770             Case Is = 3           '�p��
2771                 Break
2772             Case Is = 4           '�g���N�`�F�b�N�J�n
2773                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
2774                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342                Dly 1.0
2775                 M_Out(12342) = 0
2776                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2777                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2778                 MRet = fnMoveTorquePosi()
2779                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
2780                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2781                 Break
2782             Default
2783                 Break
2784         End Select
2785     WEnd
2786     '
2787     '�g���N�`�F�b�N����~���M
2788     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2789     '
2790     '���{�b�g�̈ʒu�����ɖ߂�
2791     '
2792     Exit Function
2793  FEnd
2794  '
2795 '
2796 '
2797 '---------------------------
2798 '
2799 '    ���C����ʂ̕\���A��\���ݒ�
2800 '         �R�����gD1001, D1002, D1003�̐ݒ�
2801 '           MWindReSet = 0     ��ʔ�\��
2802 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2803 '           MWindErrScr = 10    �G���[��� D1001, D1002
2804 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2805 '
2806 '---------------------------
2807 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2808     fnMainScreenOpen = 0
2809     '
2810    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2811         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2812     EndIf
2813     '
2814     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2815         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2816     EndIf
2817     '
2818     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2819         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2820     EndIf
2821     '
2822     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2823     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
2824     Dly 0.5
2825     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
2826     Exit Function
2827 FEnd
2828 '
2829 '��Main
2830 ''' <summary>
2831 ''' �g���N�`�F�b�N������
2832 ''' </summary>
2833 ''' <remarks>
2834 ''' Date   : 2021/12/21 : H.AJI
2835 ''' </remarks>'
2836 Function M% fnMoveTorquePosi
2837      fnMoveTorquePosi = 0
2838      Ovrd 50
2839      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
2840     '
2841     Spd M_NSpd
2842 '-------------      �h���C�o�[RST
2843     M_Out(12240)=0     '�h���C�o�[OFF CCW
2844     M_Out(12241)=0     '�h���C�o�[OFF CW
2845     M_Out(12242)=1     '�h���C�o�[���� C1
2846     M_Out(12243)=1     '�h���C�o�[���� C2
2847     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
2848 '---------------------------------------
2849 '[P-11]
2850 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
2851     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
2852     Dly 0.1
2853 '-----------------------
2854    'Cnt 0                           'Cnt����-2�@�I��
2855 '-----------------------
2856     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
2857     Dly 0.2
2858 '-----------------------
2859     ProgramBankSet(1,3)
2860     M_Out(12241)=0                   '�h���C�o�[OFF  CW
2861     'Dly 0.1
2862 '--------------------------------
2863     Ovrd 40
2864    'Dly 0.1
2865 '--------------------------------  �l�W���ߑ��x�ݒ�
2866     Spd 14                            '���C�h 100-40 100% :Spd 12
2867     Dly 0.1
2868 '--------------------------------
2869 '--------------------------------
2870 '---------------------------------�y�˂����ߓ���z
2871 '
2872     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2873    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2874     Dly 0.3                          '�������҂�
2875    M_Out(12241)=1                   '�h���C�o�[ON  CW
2876 '
2877     Wait M_In(11584)=1                '����/�G���[���o
2878     Dly 0.1
2879     Spd M_NSpd
2880    'Ovrd 20
2881     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2882     Wait M_In(11257)=1                '�l�W����SC
2883 '---------------------------------
2884     Dly 0.1
2885     M_Out(12241)=0                    '�h���C�o�[OFF CW
2886     Dly 0.1
2887     M_Out(12242)=0                    '�h���C�o�[���� C1
2888     Dly 0.1
2889     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2890     Dly 0.1
2891     M_Out(12245)=0                    '�v���O����2���� F1
2892 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2893 '
2894     Mvs PTorqueCheck,-60                       '������mov ����ύX
2895     Dly 0.1
2896 '--------------------------------------------------------------
2897    'Ovrd 80
2898 '--------------------------------------------------------------
2899 '---------------------------------------
2900 '---------------------------------------
2901 '---------------------------------------�G���[���E����
2902    *LBL1
2903    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2904    Mvs ,-100
2905    M_Out(12241)=0     '�h���C�o�[OFF CW
2906    Dly 0.1
2907    M_Out(12242)=0     '�h���C�o�[���� C1
2908    Dly 0.1
2909    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2910    Dly 0.1
2911    M_Out(12245)=0     '�v���O�������� F1
2912 '---------------------------------------
2913 '---------------------------------------
2914 '-------------
2915    'Mov PInitPos19049
2916    Dly 0.1
2917 '
2918 '
2919     Exit Function
2920 FEnd
2921 '
2922 '��Main
2923 ''' <summary>
2924 ''' �g������p�̃��C��
2925 ''' </summary>
2926 ''' <remarks>
2927 ''' Date   : 2021/07/07 : M.Hayakawa
2928 ''' </remarks>'
2929 Function Main
2930     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2931     '
2932     If M_Svo=0 Then
2933         Servo On
2934     EndIf
2935     Wait M_Svo=1
2936 '�g���X�^�[�g���t�����v���p���XON
2937     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2938 '�p�g���C�g����
2939     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2940     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2941     '
2942     M_20# = 0                                   'KEY���͏�����
2943     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
2944     MRet% = 0
2945 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2946     PActive = P_Curr                    '���݈ʒu���擾
2947     MRecoveryPass% = 0
2948     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2949         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2950             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2951             MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2952         EndIf
2953     EndIf
2954     EndIf
2955     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2956         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2957             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2958                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2959             EndIf
2960         EndIf
2961     EndIf
2962     If MRecoveryPass% = 0 Then
2963         fnInitialZone()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2964     EndIf
2965     '
2966     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2967         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2968 '�g���N�`�F�b�N
2969         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2970             MRet% = fnTorqueCheck()
2971             Break
2972         Else
2973 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
2974 '                MRtn = InspInit()               '�摜��������������
2975 '            EndIf
2976             '
2977            M_20# = MClear%                    '������
2978 '�g���J�n
2979             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2980                 MRet% = fnAssyStart()
2981             Else
2982                 M_20# = MPass%
2983             EndIf
2984 '�g���I�����t����
2985             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2986             Wait M_In(11572) = 1            '���t�擾����
2987             Dly 0.1
2988             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2989 '���t�^�[���j�b�g�ւ�OUT
2990             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2991             fnAutoScreenComment(89)         'AUTO��� �g����������
2992             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
2993 'OK/NG�t���O�o��
2994 '            If M_20# <= 0 Then
2995 '                M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
2996 '            ElseIf M_20# = MPass% Then
2997 '                M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
2998 '            EndIf
2999 'PIAS�ɑg������������
3000             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3001                 If M_20# = MPass% Then
3002                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3003                 Else
3004                     'KEY���͂�NG�̏ꍇ
3005                     If M_20# = MNgProcess% Then
3006                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3007                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3008                         MRet% = fnPiasWrite(MNG%)
3009                        nAssyNgQty = nAssyNgQty + 1
3010                     EndIf
3011                     '
3012                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3013                     If M_20# = MAssyOK% Then
3014                             '-----------------------
3015                             'D732 -> D2600 �R�s�[�v��
3016                             M_Out(12566) = 1
3017 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3018                             M_Out(12566) = 0
3019                             '
3020                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3021                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3022                             '��ԍ��ƍ�(PP�͖��g�p�j
3023 '                            MRet% = fnPCBNumberCheck()
3024                         Else
3025                             MRet% = 1
3026                         EndIf
3027                         '
3028                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3029                             If M_20# <> MAbout% Then
3030                                 '�H������OK��������
3031                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3032                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3033                                 MRet% = fnPiasWrite(MOK%)
3034                                 nAssyOkQty = 0
3035                                 nAssyOkQty = nAssyOkQty + 1
3036                             Else
3037                                 nAssyOkQty = nAssyOkQty + 1
3038                             EndIf
3039                         EndIf
3040                     EndIf
3041 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3042 '                    MRet% = fnPiasWrite(MOK%)
3043                 EndIf
3044             Else
3045                 nAssyOkQty = nAssyOkQty + 1
3046             EndIf
3047             '
3048             '�g���I�����t��������
3049             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3050             '�������A�g��OK���A�g��NG��������
3051 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3052             '
3053 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3054 '                '�摜�����I������
3055 '                MRtn = InspQuit()
3056 '            EndIf
3057         EndIf
3058         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3059     EndIf
3060 '�p�g���C�g����
3061     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3062     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3063 'GOT�\��
3064     fnAutoScreenComment(93)  'AUTO��� �H������
3065 FEnd
3066 End
3067 '
3068 '
3069 '���܂��Ȃ��R�����g
3070 '��΍폜�����
3071 '
3072 '
3073 '
3074 '
PInspPosition(1)=(+271.04,-78.15,+420.92,+160.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+271.04,+18.95,+420.92,+160.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(3)=(+334.93,+81.60,+410.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PInspPosition(4)=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(5)=(+486.73,-160.17,+397.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(6)=(+324.58,-173.46,+417.00,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
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
PTemp=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(1)=(+349.16,+91.52,+370.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(2)=(+349.16,+91.52,+313.35,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(10)=(+349.16,+91.52,+303.35,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(1)=(+233.44,+389.39,+380.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(2)=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05,+0.00,+0.00)(7,0)
PGetScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(9)=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(10)=(+233.44,+389.39,+338.70,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
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
PActive=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00)(7,0)
Pmove=(+602.00,-150.75,+380.00,+180.00,-0.02,+90.00)(7,0)
PGyroPcbRead=(+315.06,-65.41,+419.00,+160.00,+0.00,+90.00)(7,0)
PGyroPcbRead_1=(+329.62,-57.71,+450.00,-180.00,+0.00,+90.00)(7,0)
PInitialPosition=(+300.00,+0.00,+450.00,-180.00,+0.00,-180.00)(7,0)
PMainPcbRead=(+310.35,-173.29,+417.00,-180.00,+0.00,-180.00)(7,0)
PMainPcbRead_1=(+310.35,-173.29,+450.00,-180.00,+0.00,-180.00)(7,0)
PParts1Check=(+271.04,-78.15,+420.92,+160.00,+0.00,+90.00)(7,0)
PParts1Check_1=(+271.04,-78.15,+450.00,+160.00,+0.00,+90.00)(7,0)
PParts2Check=(+334.93,+81.60,+410.00,+180.00,+0.00,+180.00)(7,0)
PParts2Check_1=(+334.93,+81.60,+450.00,+180.00,+0.00,+180.00)(7,0)
PParts3Check=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00)(7,0)
PParts3Check_1=(+538.88,+54.12,+460.00,-180.00,+0.00,+180.00)(7,0)
PParts4Check=(+486.73,-160.17,+397.00,+180.00,+0.00,-180.00)(7,0)
PParts4Check_1=(+486.73,-160.17,+450.00,+180.00,+0.00,-180.00)(7,0)
PParts5Check=(+271.04,+18.95,+420.92,+160.00,+0.00,+90.00)(7,0)
PParts5Check_1=(+271.04,+18.95,+450.00,+160.00,+0.00,+90.00)(7,0)
PParts6Check=(+324.58,-173.46,+417.00,-180.00,+0.00,+180.00)(7,0)
PParts6Check_1=(+324.58,-173.46,+450.00,-180.00,+0.00,+180.00)(7,0)
PScrewFan1=(+317.38,+123.20,+303.40,-180.00,+0.00,+90.00)(7,0)
PScrewFan1_0=(+317.38,+123.20,+313.40,-180.00,+0.00,+90.00)(7,0)
PScrewFan1_1=(+317.38,+123.20,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewFan2=(+349.16,+91.52,+303.35,+180.00,+0.00,+90.00)(7,0)
PScrewFan2_0=(+349.16,+91.52,+313.35,-180.00,+0.00,+90.00)(7,0)
PScrewFan2_1=(+349.16,+91.52,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain1=(+305.16,-26.35,+312.86,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_0=(+305.16,-26.35,+318.86,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_1=(+305.16,-26.35,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain2=(+304.96,-174.86,+312.74,-180.00,+0.00,+90.00)(7,0)
PScrewMain2_0=(+304.96,-174.86,+318.74,-180.00,+0.00,+90.00)(7,0)
PScrewMain2_1=(+304.96,-174.86,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain3=(+366.92,-180.63,+312.90,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_0=(+366.92,-180.63,+318.90,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_1=(+366.92,-180.63,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain4=(+379.36,+24.74,+313.94,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_0=(+379.36,+24.74,+319.94,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_1=(+379.36,+24.74,+380.00,-180.00,+0.00,+60.00)(7,0)
PScrewMain5=(+384.95,-104.71,+330.57,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_0=(+384.95,-104.71,+341.97,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_1=(+384.95,-104.71,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain6=(+325.22,-78.83,+332.30,-180.00,+0.00,+90.00)(7,0)
PScrewMain6_0=(+325.22,-78.83,+342.70,-180.00,+0.00,+90.00)(7,0)
PScrewMain6_1=(+325.22,-78.83,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupplyFan=(+233.52,+389.28,+338.64,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyFan_1=(+233.52,+389.28,+380.00,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyFan_2=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyFan_9=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyMain=(+103.30,+195.32,+338.34,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyMain_1=(+103.30,+195.32,+380.00,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyMain_2=(+166.05,+146.93,+447.34,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyMain_9=(-3.19,+216.64,+432.44,+180.00,+0.00,-180.00)(7,0)
PTicketRead=(+602.00,-150.75,+373.00,+180.00,-0.02,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.75,+450.00,+180.00,-0.02,+90.00)(7,0)
PTorqueCheck=(+143.66,-242.00,+340.00,-180.00,-0.01,+90.00)(7,0)
PTorqueCheck_1=(+143.66,-242.00,+360.00,-180.00,-0.01,+90.00)(7,0)
