1 ' ===================================
2 '
3 '  21049001 STEP5 Assy1�v���O����
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
331 Def Inte MRetryLimit                    ' ���g���C��
332 Def Inte MRetryCount                    ' ���g���C�J�E���g
333 '
334 Dim MScwT_Case1%(2)               '����1��~�ϐ�
335 Dim MScwT_Case2%(2)               '����2��~�ϐ�
336 Dim MScwT_Case3%(2)               '����3��~�ϐ�
337 Dim MScwT_Case4%(2)               '����4��~�ϐ�
338 Dim MScwT_Case5%(2)               '����5��~�ϐ�
339 '
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
380 '
381 PCalcGetMainScrew = (+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00)  'Main�˂������@�̕␳�l
382 PCalcGetFanScrew = (+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)   'Fan�˂������@�̕␳�l
383 '
384 MRetryLimit% = 2
385 '
386 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
387 Function M% fnAssyStart
388     M_20# = MClear%                       '������
389     '�g�ݗ��ĊJ�n
390     '�v���O�������_
391     Ovrd 100
392     ' �����ʒu��ID�`�P�b�g��Ƃ��邽�ߍ폜 9/16 M.Hayakawa
393 '    Mov PInitialPosition        '���_���
394     '�����ʒu��ݒ�
395     PTemp = P_Curr
396     MRtn = 0
397     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
398         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
399             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
400                 MRtn = 1
401                 Break
402             EndIf
403             Break
404         EndIf
405         Break
406     EndIf
407     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
408     If MRtn = 1 Then
409         M_Out(12256) = 1 Dly 0.3            '�ʒu���ߏoON
410         Mov PTicketRead
411         Break
412     Else
413         Mov PInitialPosition
414         M_Out(12256) = 1 Dly 0.3           '�ʒu���ߏoON
415         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
416         Mvs PTicketRead             'ID�ǂ݈ʒu
417         Break
418     EndIf
419     *RE_PUSH
420 '    If M_20# = MContinue% Then M_Out(12257) = 0
421     If M_20# = MContinue% Then M_Out(12256) = 1 Dly 0.3
422     If M_20# = MContinue% Then M_20# = MClear%
423     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
424     If MRtn = 1 Then GoTo *CompPush
425         M_Out(12257) = 1 Dly 0.3    ' Y71 1:�ʒu����CY ����
426         fErrorProcess(11,231,282,0)
427     If M_20# = MNext% Then M_Out(12256) = 1 Dly 0.3 'Y70 1:�ʒu����CY �Œ�
428     If M_20# = MNext% Then M_20# = MClear%
429     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
430     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
431     If M_20# = MContinue% Then GoTo *RE_PUSH
432     *CompPush
433 '
434     *RE_READ
435     If M_20# = MContinue% Then M_20# = MClear%
436 '
437     MRtn = 1                            'MRtn������
438     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
439         MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
440     EndIf
441         '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
442         '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
443 '
444     If MRtn = 1 Then GoTo *CompRead
445     Mvs PTicketRead_1                       ' ��U���ɑҔ� �ǉ� 22/07/16 M,H
446     'fErrorProcess(11,97,25,0)
447     If M_20# = MPass% Then GoTo *AssyEnd    ' ���ւ������ꂽ���̃R�����g�����{�W�����v��ύX 2022/07/20 M.H
448     If M_20# = MNext% Then M_20# = MClear%
449     If M_20# = MAbout% Then GoTo *AssyEnd       ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
450     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
451     If M_20# = MContinue% Then GoTo *RE_READ
452 '    If M_20# = MNext% Then M_20# = MPass%
453     GoTo *ASSY_ERROR_END
454     *CompRead
455     '
456 '�yMAIN���ID�ǂݍ��݁z
457     *RE_MEIN_CHECK
458     PInspPosition(1) = PMainPcbRead 'MAIN��Ǎ��ʒu
459     MInspGroup%(1) = 2              '����G�ԍ�
460     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
461 '
462     If MRtn = 1 Then GoTo *CompMainCheck
463     fErrorProcess(11,38,25,0)
464     If M_20# = MNext% Then M_20# = MClear%
465     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
466     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
467     If M_20# = MContinue% Then GoTo *RE_MEIN_CHECK
468     *CompMainCheck
469 '�yGYRO���ID�ǂݍ��݁z
470     *RE_GYRO_CHECK
471     PInspPosition(1) = PGyroPcbRead 'GYRO��Ǎ��ʒu
472     MInspGroup%(1) = 3              '����G�ԍ�
473     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
474 '
475     If MRtn = 1 Then GoTo *CompGyroCheck
476     fErrorProcess(11,38,25,0)
477     If M_20# = MNext% Then M_20# = MClear%
478     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
479     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
480     If M_20# = MContinue% Then GoTo *RE_GYRO_CHECK
481     *CompGyroCheck
482 '�y���ID�R�s�[�z
483     *RE_PCB_RECORD
484     M_Out(12571) = 1    ' �̈�1 ��ԍ��R�s�[ (D2600-) On
485     Dly 0.1
486     M_Out(12572) = 1    ' �̈�2 ��ԍ��R�s�[ (D2612-) On
487     Dly 0.1
488     M_Out(12566) = 1    ' toPLC_��ԍ��R�s�[�v�� On
489 '
490     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��R�s�[���� On
491     If MRtn = 1 Then
492         M_Out(12571) = 0  ' �̈�1 ��ԍ��R�s�[ (D2600-) Off
493         Dly 0.1
494         M_Out(12572) = 0  ' �̈�2 ��ԍ��R�s�[ (D2612-) Off
495         Dly 0.1
496         M_Out(12566) = 0  ' toPLC_��ԍ��R�s�[�v�� Off
497 '        GoTo *RE_PCB_COMPAIRE   ' ��ԍ��ƍ��ɃX�L�b�v
498     Else
499         fErrorProcess(11,39,25,0)
500         If M_20# = MNext% Then M_20# = MClear%
501         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
502         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
503         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
504     EndIf
505 '�y���ID�ƍ��i�R�t���j�z
506     MRetryCount% = 0
507     While (MRetryCount% <= MRetryLimit%)
508         *RE_PCB_COMPAIRE
509         M_Out(12557)= 1 ' ��ԍ��ƍ��r�b�gON
510         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��ƍ�OK(M420) On
511         If MRtn = 1 Then
512             M_Out(12557)= 0     ' ��ԍ��ƍ��r�b�gOff
513             ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
514             MRetryCount% = 99
515         Else
516             If MRetryCount% = MRetryLimit% Then
517                 If M_In(11565) = 1 Then
518                     fErrorProcess(11,37,25,0)
519                 Else
520                     fErrorProcess(11,38,25,0)
521                 EndIf
522                 If M_20# = MNext% Then
523                     M_20# = MClear%
524                     ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
525                     MRetryCount% = 99
526                 EndIf
527                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
528                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
529                 If M_20# = MContinue% Then
530                     MRetryCount% = 0
531                 EndIf
532             Else
533                 ' ���g���C�񐔃C���N�������g
534                 MRetryCount% = MRetryCount% + 1
535                 Dly 0.1  ' ���̍H���ƃ^�C�~���O�����炷�ׂ̃f�B���C
536             EndIf
537         EndIf
538     WEnd
539 '
540     *RE_CHECK
541     PInspPosition(1) = PParts1Check '���i1�摜�`�F�b�N�ʒu(MAIN����Ӂj
542     MInspGroup%(1) = 4              '����G�ԍ�
543     PInspPosition(2) = PParts2Check '���i2�摜�`�F�b�N�ʒu�i�w�ʔ��Ӂj
544     MInspGroup%(2) = 5              '����G�ԍ�
545 '    PInspPosition(3) = PParts3Check '���i3�摜�`�F�b�N�ʒu�iSOC����Ӂj
546 '    MInspGroup%(3) = 6              '����G�ԍ�
547 '    PInspPosition(4) = PParts4Check '���i4�摜�`�F�b�N�ʒu�i�����Ӂj
548 '    MInspGroup%(4) = 7              '����G�ԍ�
549     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 2, -1, 1 )  '�摜�����������s
550     If MRtn = 1 Then GoTo *CompCheck
551     fErrorProcess(11,43,23,0)
552     If M_20# = MNext% Then M_20# = MClear%
553     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
554     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
555     If M_20# = MContinue% Then GoTo *RE_CHECK
556     *CompCheck
557     '
558     MRtn = FnCtlValue2(1)          '�������{�P  2022/04/28 �n��
559     '���i�ʒu����(ID�Ǎ���ɕύX 9/16 M.Hayakawa�j
560     *RE_POS
561     If M_20# = MContinue% Then M_20# = MClear%
562     M_Out(12256)=1 Dly 0.3      '�ʒu����CY�pSV�o�[�p���X�o��
563     MRtn = FnCtlValue2(99)       '�Ǐ��J�n�M��OFF  2022/04/28 �n��
564 '
565     'Wait M_In(11266)=1          '�ʒu���ߏo�[���o�C���ɂ��R�����g�A�E�g(8/26����))
566     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
567     If MRtn = 1 Then GoTo *Comp_Pos_1
568     fErrorProcess(11,231,282,0)
569     If M_20# = MNext% Then M_20# = MClear%
570     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
571     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
572     If M_20# = MContinue% Then GoTo *RE_POS
573     *Comp_Pos_1
574     '
575     M_Out(12258)=1 Dly 0.3      '�v�b�V��CY�pSV�o�[�p���X�o��(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
576     M_Out(12260)=1 Dly 0.3      'FAN�N�����v�ߒ[�p���X�o��(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
577     Mov PScrewSupplyMain_1
578 '
579 '    M_Out(12258)=1 Dly 0.3      '�v�b�V��CY�pSV�o�[�p���X�o��
580     'Wait M_In(11268)=1          '�v�b�V���o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
581     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '�v�b�V���o�[���o
582     If MRtn = 1 Then GoTo *Comp_Pos_2
583     fErrorProcess(11,231,282,0)
584     If M_20# = MNext% Then M_20# = MClear%
585     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
586     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
587     If M_20# = MContinue% Then GoTo *RE_POS
588     *Comp_Pos_2
589     '
590 '    M_Out(12260)=1 Dly 0.3      'FAN�N�����v�ߒ[�p���X�o��(���C���˂����ߌ�ɕύX M.Hayakawa)(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
591     'Wait M_In(11270)=1          'FAN�N�����v�ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
592     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'FAN�N�����v�ߒ[���o(8/26����)
593     If MRtn = 1 Then GoTo *Comp_Pos_3
594     fErrorProcess(11,231,282,0)
595     If M_20# = MNext% Then M_20# = MClear%
596     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
597     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
598     If M_20# = MContinue% Then GoTo *RE_POS
599     *Comp_Pos_3
600     '
601     '
602     'Main��̃l�W����
603     'Main��p�l�W�����@�փl�W�����ɍs��
604     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
605     '
606     '*ScrewSupplyMain           '�ꎞ�R�����g�A�E�g(�ȉ�5�s,8/5����)
607 '    Mov PScrewSupplyMain_2      '�l�W�����@���_
608 '    Mov PScrewSupplyMain_1      '�l�W�s�b�N�A�b�v���
609 '    Mvs PScrewSupplyMain        '�l�W�s�b�N�A�b�v
610 '    Mvs PScrewSupplyMain_1      '�l�W�s�b�N�A�b�v���
611 '    Mov PScrewSupplyMain_2      '�l�W�����@���_
612     'Return                     '�ꎞ�R�����g�A�E�g(8/4����)
613     'ScrewPositionDebug_1()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
614     '
615     PGetScrewPosTemp(1) = PScrewSupplyMain_1   '�l�W�s�b�N�A�b�v������(8/26����)
616     PGetScrewPosTemp(2) = PScrewSupplyMain_2   '�l�W�������_����(8/26����)
617     PGetScrewPosTemp(9) = PScrewSupplyMain_9   '�l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
618     PGetScrewPosTemp(10) = PScrewSupplyMain    '�l�W�s�b�N�A�b�v����(8/26����)
619     '
620     *RE_SCREW_GET_1                                '���g���C�p���x��
621     If M_20# = MContinue% Then M_20# = MClear%
622     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
623     If M_20# = MClear% Then GoTo *Comp_Screw_1
624     If M_20# = MNext% Then M_20# = MClear%
625     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
626     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
627     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
628     *Comp_Screw_1
629     '
630     '�@�ԃl�W����
631 '    Mov PScrewMain1_1           '�@���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
632 '    Ovrd 5
633 '    Mvs PScrewMain1             '�@�l�W����
634 '    Ovrd 10
635 '    Mvs PScrewMain1_1           '�@���
636     PScrewPosTemp(1) = PScrewMain1_1    '�l�W1���ߊJ�n�ʒu������(8/26����)
637     PScrewPosTemp(2) = PScrewMain1_0    '�l�W1���ߊJ�n�ʒu����(8/26����)
638     PScrewPosTemp(10) = PScrewMain1     '�l�W1���ߏI���ʒu����(8/26����)
639     M_Out16(12672) = 1                  '�l�W���߈ʒu�ԍ����M
640     MRtn = ScrewTight(PScrewPosTemp,1,10.0)    '�l�W1���߂̎��s(8/26����)
641     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
642     If MRtn = 1 Then GoTo *CompScrew1
643     Mov PInitialPosition
644     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
645     MScrewErrorCord% = MScrewErrorCord% + 1
646     If MRtn = 0 Then
647         fErrorProcess(11,MScrewErrorCord%,52,0)
648     ElseIf MRtn = -1 Then
649         fErrorProcess(11,94,95,0)
650     EndIf
651 '    fErrorProcess(11,53,52,0)
652     If M_20# = MNext% Then M_20# = MClear%
653     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
654     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
655     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
656     *CompScrew1
657     '
658     'Main��p�l�W�����@�փl�W�����ɍs��
659     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
660     'ScrewPositionDebug_1()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
661     *RE_SCREW_GET_2                                '���g���C�p���x��
662     If M_20# = MContinue% Then M_20# = MClear%
663     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
664     If M_20# = MClear% Then GoTo *Comp_Screw_2
665     If M_20# = MNext% Then M_20# = MClear%
666     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
667     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
668     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
669     *Comp_Screw_2
670     '�A�ԃl�W����
671 '    Mov PScrewMain2_1           '�A���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
672 '    Ovrd 5
673 '    Mvs PScrewMain2             '�A�l�W����
674 '    Ovrd 10
675 '    Mvs PScrewMain2_1           '�A���
676     PScrewPosTemp(1) = PScrewMain2_1    '�l�W2���ߊJ�n�ʒu������(8/26����)
677     PScrewPosTemp(2) = PScrewMain2_0    '�l�W2���ߊJ�n�ʒu����(8/26����)
678     PScrewPosTemp(10) = PScrewMain2     '�l�W1���ߏI���ʒu����(8/26����)
679     M_Out16(12672) = 2                  '�l�W���߈ʒu�ԍ����M
680     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����2�̎��s(8/26����)
681     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
682     If MRtn = 1 Then GoTo *CompScrew2
683     Mov PInitialPosition
684     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
685     MScrewErrorCord% = MScrewErrorCord% + 2
686     If MRtn = 0 Then
687         fErrorProcess(11,MScrewErrorCord%,52,0)
688     ElseIf MRtn = -1 Then
689         fErrorProcess(11,94,95,0)
690     EndIf
691 '    fErrorProcess(11,54,52,0)
692     If M_20# = MNext% Then M_20# = MClear%
693     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
694     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
695     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
696     *CompScrew2
697     '
698     'Main��p�l�W�����@�փl�W�����ɍs��
699     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
700     *RE_SCREW_GET_3                                '���g���C�p���x��
701     If M_20# = MContinue% Then M_20# = MClear%
702     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
703     If M_20# = MClear% Then GoTo *Comp_Screw_3
704     If M_20# = MNext% Then M_20# = MClear%
705     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
706     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
707     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
708     *Comp_Screw_3
709     '�B�ԃl�W����
710 '    Mov PScrewMain3_1           '�B���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
711 '    Ovrd 5
712 '    Mvs PScrewMain3             '�B�l�W����
713 '    Ovrd 10
714 '    Mvs PScrewMain3_1           '�B���
715     PScrewPosTemp(1) = PScrewMain3_1    '�l�W3���ߊJ�n�ʒu������(8/26����)
716     PScrewPosTemp(2) = PScrewMain3_0    '�l�W3���ߊJ�n�ʒu����(8/26����)
717     PScrewPosTemp(10) = PScrewMain3     '�l�W3���ߏI���ʒu����(8/26����)
718     M_Out16(12672) = 3                  '�l�W���߈ʒu�ԍ����M
719     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����3�̎��s(8/26����)
720     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
721     If MRtn = 1 Then GoTo *CompScrew3
722     Mov PInitialPosition
723     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
724     MScrewErrorCord% = MScrewErrorCord% + 3
725     If MRtn = 0 Then
726         fErrorProcess(11,MScrewErrorCord%,52,0)
727     ElseIf MRtn = -1 Then
728         fErrorProcess(11,94,95,0)
729     EndIf
730 '    fErrorProcess(11,55,52,0)
731     If M_20# = MNext% Then M_20# = MClear%
732     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
733     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
734     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
735     *CompScrew3
736     '
737     'Main��p�l�W�����@�փl�W�����ɍs��
738     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
739     *RE_SCREW_GET_4                                '���g���C�p���x��
740     If M_20# = MContinue% Then M_20# = MClear%
741     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
742     If M_20# = MClear% Then GoTo *Comp_Screw_4
743     If M_20# = MNext% Then M_20# = MClear%
744     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
745     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
746     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
747     *Comp_Screw_4
748     '�C�ԃl�W����
749 '    Mov PScrewMain4_1           '�C���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
750 '    Ovrd 5
751 '    Mvs PScrewMain4             '�C�l�W����
752 '    Ovrd 10
753 '    Mvs PScrewMain4_1           '�C���
754     PScrewPosTemp(1) = PScrewMain4_1    '�l�W4���ߊJ�n�ʒu������(8/26����)
755     PScrewPosTemp(2) = PScrewMain4_0    '�l�W4���ߊJ�n�ʒu����(8/26����)
756     PScrewPosTemp(10) = PScrewMain4     '�l�W4���ߏI���ʒu����(8/26����)
757     M_Out16(12672) = 4                  '�l�W���߈ʒu�ԍ����M
758     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����4�̎��s(8/26����)
759     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
760     If MRtn = 1 Then GoTo *CompScrew4
761     Mov PInitialPosition
762     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
763     MScrewErrorCord% = MScrewErrorCord% + 4
764     If MRtn = 0 Then
765         fErrorProcess(11,MScrewErrorCord%,52,0)
766     ElseIf MRtn = -1 Then
767         fErrorProcess(11,94,95,0)
768     EndIf
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
796     M_Out16(12672) = 5                  '�l�W���߈ʒu�ԍ����M
797     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        '�l�W����5�̎��s(8/26����)
798     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
799     If MRtn = 1 Then GoTo *CompScrew5
800     Mov PInitialPosition
801     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
802     MScrewErrorCord% = MScrewErrorCord% + 5
803     If MRtn = 0 Then
804         fErrorProcess(11,MScrewErrorCord%,52,0)
805     ElseIf MRtn = -1 Then
806         fErrorProcess(11,94,95,0)
807     EndIf
808 '    fErrorProcess(11,57,52,0)
809     If M_20# = MNext% Then M_20# = MClear%
810     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
811     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
812     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
813     *CompScrew5
814     '
815     'Main��p�l�W�����@�փl�W�����ɍs��
816     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
817 '�ȉ�3�sPP�i�Ƀl�W�����Ȃ����߈ꎞ�폜 9/16 M.Hayakawa
818     *RE_SCREW_GET_6                                '���g���C�p���x��
819     If M_20# = MContinue% Then M_20# = MClear%
820     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
821     If M_20# = MClear% Then GoTo *Comp_Screw_6
822     If M_20# = MNext% Then M_20# = MClear%
823     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
824     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
825     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
826     *Comp_Screw_6
827     '�E�ԃl�W����
828 '    Mov PScrewMain6_1           '�E���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
829 '    Ovrd 5
830 '    Mvs PScrewMain6             '�E�l�W����
831 '    Ovrd 10
832 '    Mvs PScrewMain6_1           '�E���
833 '�ȉ�3�sPP�i�Ƀl�W�����Ȃ����߈ꎞ�폜 9/16 M.Hayakawa
834     PScrewPosTemp(1) = PScrewMain6_1    '�l�W6���ߊJ�n�ʒu������(8/26����)
835     PScrewPosTemp(2) = PScrewMain6_0    '�l�W6���ߊJ�n�ʒu����(8/26����)
836     PScrewPosTemp(10) = PScrewMain6     '�l�W6���ߏI���ʒu����(8/26����)
837     M_Out16(12672) = 6                  '�l�W���߈ʒu�ԍ����M
838     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        '�l�W����6�̎��s(8/26����)
839     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
840     If MRtn = 1 Then GoTo *CompScrew6
841     Mov PInitialPosition
842     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
843     MScrewErrorCord% = MScrewErrorCord% + 6
844     If MRtn = 0 Then
845         fErrorProcess(11,MScrewErrorCord%,52,0)
846     ElseIf MRtn = -1 Then
847         fErrorProcess(11,94,95,0)
848     EndIf
849 '    fErrorProcess(11,58,52,0)
850     If M_20# = MNext% Then M_20# = MClear%
851     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
852     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
853     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
854     *CompScrew6
855     '
856     'FAN�p�l�W�����@�փl�W�����ɍs��
857     'GoSub *ScrewSupplyFan      '�ꎞ�R�����g�A�E�g(8/4����)
858 ' �l�W�ʒu�w��O�Ɏ��ɂ����Ă���H 1�s�ꎞ�폜 9/16 M.Hayakawa
859 '    MRtn = ScrewGet(PGetScrewPosTemp)       '�l�W�����ɍs��(8/26����)
860     '
861 '    *ScrewSupplyFan
862 '    Mov PScrewSupplyFan_2       '�l�W�����@���_
863 '    Mov PScrewSupplyFan_1       '�l�W�s�b�N�A�b�v���
864 '    Mvs PScrewSupplyFan         ''�l�W�s�b�N�A�b�v
865 '    Mvs PScrewSupplyFan_1       '�l�W�s�b�N�A�b�v���
866 '    Mov PScrewSupplyFan_2       '�l�W�����@���_
867    ' Return                     '�ꎞ�R�����g�A�E�g(8/4����)
868     'ScrewPositionDebug_2()       '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
869     '
870     PGetScrewPosTemp(1) = PScrewSupplyFan_1   '�l�W�s�b�N�A�b�v������(8/26����)
871     PGetScrewPosTemp(2) = PScrewSupplyFan_2   '�l�W�������_����(8/26����)
872     PGetScrewPosTemp(9) = PScrewSupplyFan_9   '�l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
873     PGetScrewPosTemp(10) = PScrewSupplyFan    '�l�W�s�b�N�A�b�v����(8/26����)
874 '
875     *RE_SCREW_GET_7                                '���g���C�p���x��
876 '
877     If M_20# = MContinue% Then M_20# = MClear%
878     ScrewGet(PGetScrewPosTemp , 11260 , 0)          '�l�W�󂯎��J�n
879     If M_20# = MClear% Then GoTo *Comp_Screw_7
880     If M_20# = MNext% Then M_20# = MClear%
881     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
882     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
883     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
884     *Comp_Screw_7
885     '�F�ԃl�W����
886 '    Mov PScrewFan1_1            '�F���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
887 '    Ovrd 5
888 '    Mvs PScrewFan1              '�F�l�W����
889 '    Ovrd 10
890 '    Mvs PScrewFan1_1            '�F���
891     PScrewPosTemp(1) = PScrewFan1_1    'Fan1�l�W���ߊJ�n�ʒu������(8/26����)
892     PScrewPosTemp(2) = PScrewFan1_0    'Fan1�l�W���ߊJ�n�ʒu����(8/26����)
893     PScrewPosTemp(10) = PScrewFan1     'Fan1�l�W���ߏI���ʒu����(8/26����)
894     M_Out16(12672) = 7                  '�l�W���߈ʒu�ԍ����M
895     MRtn = ScrewTight(PScrewPosTemp,2,10.0)       'Fan�l�W����1�̎��s(8/26����)���葬�x6.7����10.0��7/27����
896     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
897     If MRtn = 1 Then GoTo *CompScrew7
898     Mov PInitialPosition
899     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
900     MScrewErrorCord% = MScrewErrorCord% + 7
901     If MRtn = 0 Then
902         fErrorProcess(11,MScrewErrorCord%,52,0)
903     ElseIf MRtn = -1 Then
904         fErrorProcess(11,94,95,0)
905     EndIf
906 '    fErrorProcess(11,59,52,0)
907     If M_20# = MNext% Then M_20# = MClear%
908     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
909     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
910     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
911     *CompScrew7
912 '
913     '
914     'FAN�p�l�W�����@�փl�W�����ɍs��
915     'GoSub *ScrewSupplyFan      '�ꎞ�R�����g�A�E�g(8/4����)
916     'ScrewPositionDebug_2()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
917     *RE_SCREW_GET_8                                '���g���C�p���x��
918     If M_20# = MContinue% Then M_20# = MClear%
919     ScrewGet(PGetScrewPosTemp , 11260 , 0)          '�l�W�󂯎��J�n
920     If M_20# = MClear% Then GoTo *Comp_Screw_8
921     If M_20# = MNext% Then M_20# = MClear%
922     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
923     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
924     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
925     *Comp_Screw_8
926     '�G�ԃl�W����
927 '    Mov PScrewFan2_1            '�G���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
928 '    Ovrd 5
929 '    Mvs PScrewFan2              '�G�l�W����
930 '    Ovrd 10
931 '    Mvs PScrewFan2_1            '�G���
932     PScrewPosTemp(1) = PScrewFan2_1    'Fan2�l�W���ߊJ�n�ʒu������(8/26����)
933     PScrewPosTemp(2) = PScrewFan2_0    'Fan2�l�W���ߊJ�n�ʒu����(8/26����)
934     PScrewPosTemp(10) = PScrewFan2     'Fan2�l�W���ߏI���ʒu����(8/26����)
935     M_Out16(12672) = 8                  '�l�W���߈ʒu�ԍ����M
936     MRtn = ScrewTight(PScrewPosTemp,2,10.0)       'Fan�l�W����2�̎��s(8/26����)���葬�x6.7����10.0��7/27����
937     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
938     If MRtn = 1 Then GoTo *CompScrew8
939     Mov PInitialPosition
940     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
941     MScrewErrorCord% = MScrewErrorCord% + 8
942     If MRtn = 0 Then
943         fErrorProcess(11,MScrewErrorCord%,52,0)
944     ElseIf MRtn = -1 Then
945         fErrorProcess(11,94,95,0)
946     EndIf
947     If M_20# = MNext% Then M_20# = MClear%
948     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
949     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
950     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
951     *CompScrew8
952 '
953     '�v���O�������_
954     'Mov PInitialPosition        ' ���_���
955     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
956     Mov PTicketRead_1           ' �`�P�b�g���[�h�ʒu
957     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
958     InitialState()              ' ������Ԃɂ���
959     M_20# = MAssyOK%              ' ����I������
960     GoTo *fnAssyStart_FEndPosi
961 '
962 *ASSY_ERROR_END
963     fnInitialZone()   ' �����ʒu�Ɉړ�
964 *AssyEnd
965     InitialState()  ' ������Ԃɂ���
966 *fnAssyStart_FEndPosi
967     Exit Function
968 FEnd
969 '
970 '��fnPiasCheck
971 ''' <summary>
972 ''' PIAS�`�P�b�g�Ǎ���
973 ''' </summary>
974 ''' <returns>   0 : NG
975 '''             1 : OK(�Ǎ��݊���)
976 ''' </returns>
977 ''' <remarks>
978 ''' Date   : 2021/07/07 : M.Hayakawa
979 ''' </remarks>'
980 ''' <Update>
981 ''' Date   : 2022/01/11 : ����
982 ''' </Update>
983 Function M% fnPiasCheck
984     fnPiasCheck = 0
985     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
986     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����
987 '
988 *RETRY_PIAS
989     M_20# = MClear%
990     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
991     '
992     '�yID�`�P�b�g�ǂݍ��݁z
993     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
994     MInspGroup%(1) = 1              '����G�ԍ�
995     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
996 '
997     '�G���[�̏ꍇ
998     If MRtn <> 1 Then
999         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1000         If MRtn <> 1 Then
1001             'D720 -> D1300 �R�s�[�v��
1002             M_Out(12565) = 1
1003             Dly 0.5
1004             M_Out(12565) = 0
1005             '�G���[�����L�q
1006             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1007             'GOT KEY���͑҂�
1008             MKeyNumber = fnKEY_WAIT()
1009             '
1010             Select MKeyNumber
1011                 Case MNext%         '���ւ�I�������ꍇ
1012                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1013                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1014                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1015                     Break
1016                 Case MAbout%        '��~��I�������ꍇ
1017                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1018                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1019                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1020                     Break
1021                 Case MNgProcess%    'NG��I�������ꍇ
1022                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1023                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1024                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1025                     Break
1026                 Case MContinue%     '�p����I�������ꍇ
1027                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1028                     M_20# = MContinue%
1029                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1030                     Break
1031             End Select
1032         EndIf
1033     EndIf
1034     If M_20# = MPass% Then GoTo *fnPiasCheck_End            'PIAS�`�F�b�N�I��
1035     If M_20# = MAbout% Then GoTo *fnPiasCheck_End           'PIAS�`�F�b�N�I��
1036     If M_20# = MNgProcess% Then GoTo *fnPiasCheck_End       'PIAS�`�F�b�N�I��
1037     If M_20# = MContinue% Then GoTo *RETRY_PIAS             'PIAS�`�F�b�N���g���C
1038 '----------D720 -> D1300 �R�s�[�v��----------
1039     M_Out(12565) = 1
1040     Dly 0.5
1041     M_Out(12565) = 0
1042 '----------�ʐM�m�F������----------
1043     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1044     MRtn = 0                ' ������
1045     M_20# = MClear%         ' ������
1046     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1047     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1048 '    If MRtn <> 1 Then
1049 '        If M_20# = MContinue% Then
1050 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1051 '        Else
1052 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1053 '        EndIf
1054 '    EndIf
1055     If MRtn = 1 Then GoTo *PCComu_OK                '�ʐMOK�����x���փW�����v
1056     If M_20# = MContinue% Then GoTo *RETRY_PIAS      '�`�P�b�g�ǂݒ������烊�g���C
1057     GoTo *fnPiasCheck_End                           '���̑��̏ꍇPIAS�`�F�b�N�I��
1058     *PCComu_OK
1059 '----------�H�������m�F----------
1060     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1061     MRtn = 0                ' ������
1062     M_20# = MClear%         ' ������
1063     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1064     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1065 '    If MRtn <> 1 Then
1066 '        If M_20# = MContinue% Then
1067 '            GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1068 '        Else
1069 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1070 '        EndIf
1071 '    EndIf
1072     If MRtn = 1 Then GoTo *ProcessCheck_OK                '�H���`�F�b�NOK�����x���փW�����v
1073     If M_20# = MContinue% Then GoTo *RETRY_PIAS      '�`�P�b�g�ǂݒ������烊�g���C
1074     GoTo *fnPiasCheck_End                           '���̑��̏ꍇPIAS�`�F�b�N�I��
1075     *ProcessCheck_OK
1076     '
1077     fnPiasCheck = 1
1078     *fnPiasCheck_End
1079     Exit Function
1080 FEnd
1081 '
1082 '��fnPCComuCheck
1083 ''' <summary>
1084 ''' PC-PLC�ʐM�`�F�b�N
1085 ''' </summary>
1086 ''' <returns>   0 : NG
1087 '''             1 : OK(�Ǎ��݊���)
1088 ''' </returns>
1089 ''' <remarks>
1090 ''' Date   : 2021/07/07 : M.Hayakawa
1091 ''' </remarks>'
1092 Function M% fnPCComuCheck
1093     fnPCComuCheck = 0
1094     MJudge% = 0                                  '������
1095     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1096     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1097     '
1098     For MStaNo = 0 To 5
1099         '
1100         If M_In(MIN_PIAS_ComOK%) = 1 Then
1101             'PC�ʐMOK(M400)
1102             MJudge% = MOK%
1103             MStaNo = 5
1104             Break
1105         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1106             'toRBT_�ʐM�m�Ftime out
1107             MJudge% = MNG%
1108             MCommentD1001 = 15
1109             MCommentD1002 = 21
1110             MStaNo = 5
1111             Break
1112         Else
1113             'toRBT_�ʐM�m�Ftime out
1114             MJudge% = MNG%
1115             MCommentD1001 = 14
1116             MCommentD1002 = 21
1117             Break
1118         EndIf
1119     Next MStaNo
1120     '
1121     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1122     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1123     '
1124     '�G���[���
1125     If MJudge% <> MOK% Then
1126         M_20# = MClear%     '������
1127         '�G���[�����L�q
1128         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1129         'GOT KEY���͑҂�
1130         MKeyNumber = fnKEY_WAIT()
1131         '
1132         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1133             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1134             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1135             Break
1136         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1137             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1138             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1139             Break
1140         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1141             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1142             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1143             Break
1144         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1145             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1146             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1147             Break
1148         EndIf
1149     Else
1150         'OK�̏ꍇ
1151         fnPCComuCheck = 1
1152     EndIf
1153     Exit Function
1154 FEnd
1155 '
1156 '��fnProcessCheck
1157 ''' <summary>
1158 ''' �H�������m�F
1159 ''' </summary>
1160 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1161 '''             -1�F�O�H������NG  -2�F���H����������
1162 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1163 '''             -5�F���������G���[
1164 ''' </returns>
1165 ''' <remarks>
1166 ''' Date   : 2021/07/07 : M.Hayakawa
1167 ''' </remarks>'
1168 Function M% fnProcessCheck
1169     fnProcessCheck = 0
1170     MJudge% = MNG%      '��UNG���������Ƃ���
1171 '----------�H�������m�F----------
1172     MCommentD1001 = 0   '�R�����g������
1173     For MStaNo = 0 To 5
1174         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1175         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1176         '
1177         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1178             MJudge% = MOK%
1179             fnAutoScreenComment(85)     ' AUTO���
1180             MStaNo = 5
1181             Break
1182         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1183             MFlgLoop% = 0
1184             MJudge% = MNG%
1185             MCommentD1001 = 27
1186             MCommentD1002 = 22
1187             fnAutoScreenComment(94)     ' AUTO���
1188             fnProcessCheck = -2         ' NG��-2��Ԃ�
1189             MStaNo = 5
1190             Break
1191         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1192            MJudge% = MNG%
1193             MCommentD1001 = 31
1194             MCommentD1002 = 22
1195             fnAutoScreenComment(83)     ' AUTO���
1196             fnProcessCheck = -3         ' NG��-3��Ԃ�
1197             MStaNo = 5
1198             Break
1199         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1200             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1201             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1202             MJudge% = MNG%
1203             MCommentD1001 = 32
1204             MCommentD1002 = 22
1205             fnAutoScreenComment(84)     ' AUTO���
1206             fnProcessCheck = -1         ' NG��-1��Ԃ�
1207             Dly 1.0
1208             '�H�������m�FOFF
1209             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1210             Dly 1.0
1211            'MStaNo = 5
1212             Break
1213         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1214             MFlgLoop% = 0
1215             MJudge% = MNG%
1216             MCommentD1001 = 29
1217             MCommentD1002 = 22
1218             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1219             fnProcessCheck = -5         ' NG��-5��Ԃ�
1220             MStaNo = 5
1221             Break
1222         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1223             MJudge% = MNG%
1224             If MCommentD1001 = 32 Then
1225                 '�������Ȃ�
1226             Else
1227                 MCommentD1001 = 26
1228             EndIf
1229             MCommentD1002 = 22
1230             fnProcessCheck = -4         ' NG��-4��Ԃ�
1231             MStaNo = 5
1232             Break
1233         Else
1234             MJudge% = MNG%
1235             MCommentD1001 = 28
1236             MCommentD1002 = 22
1237         EndIf
1238     Next MStaNo
1239     '�H�������m�FOFF
1240     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1241     '�ʉߗ���NG �H�������̏ꍇ
1242     If MJudge% = MPass% Then
1243         M_20# = MPass%
1244     EndIf
1245     '
1246     '�G���[���
1247     If MJudge% <> MOK% Then
1248         M_20# = MClear%     '������
1249         '�G���[�����L�q
1250         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1251         'GOT KEY���͑҂�
1252         MKeyNumber = fnKEY_WAIT()
1253         '
1254         Select MKeyNumber
1255             Case MAbout%        '��~��I�������ꍇ
1256                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1257                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1258                 Break
1259             Case MNext%         '���ւ�I�������ꍇ
1260                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1261                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1262                 Break
1263             Case MContinue%     '�p����I�������ꍇ
1264                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1265                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1266                 Break
1267             Case MNgProcess%    'NG��I�������ꍇ
1268                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1269                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1270                 Break
1271         End Select
1272     Else
1273         fnProcessCheck = 1  ' OK��1��Ԃ�
1274     EndIf
1275     Exit Function
1276 FEnd
1277 '
1278 '��fnPiasWrite
1279 ''' <summary>
1280 ''' Pias �g�����ʏ����ݗv��
1281 ''' </summary>
1282 '''<param name="MFlg%">
1283 ''' MOK%(1) = �H��������OK��������
1284 ''' MNG%(0) = �H��������NG��������
1285 '''</param>
1286 '''<returns></returns>
1287 ''' <remarks>
1288 ''' Date   : 2021/07/07 : M.Hayakawa
1289 ''' </remarks>'
1290 Function M% fnPiasWrite(ByVal MFlg%)
1291       fnPiasWrite = 0
1292 *RETRY_PIASWRITE
1293     '
1294     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1295    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1296     If MFlg% = MOK% Then
1297         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1298     Else
1299         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1300     EndIf
1301     Dly 0.1                  '�O�̂���
1302     '
1303     'Pias�֏����݊J�n M305 -> ON
1304     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1305     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1306     '
1307     MJudge% = MNG%
1308     '
1309     For MStaNo = 0 To 5
1310         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1311             MJudge% = MOK%
1312             'MRet = fnAutoScreenComment(85)  'AUTO���
1313             MStaNo = 5
1314             Break
1315         '
1316         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1317             MJudge% = MNG%
1318             'MRet = fnAutoScreenComment(85)  'AUTO���
1319            MCommentD1001 = 34
1320            MCommentD1002 = 25
1321             MStaNo = 5
1322             Break
1323         '
1324         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1325             MJudge% = MNG%
1326             'MRet = fnAutoScreenComment(85)  'AUTO���
1327            MCommentD1001 = 35
1328            MCommentD1002 = 25
1329             MStaNo = 5
1330             Break
1331         '
1332         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1333             MJudge% = MNG%
1334             'MRet = fnAutoScreenComment(85)  'AUTO���
1335            MCommentD1001 = 36
1336            MCommentD1002 = 25
1337             MStaNo = 5
1338             Break
1339         '
1340         Else
1341             MJudge% = MNG%
1342            MCommentD1001 = 42
1343            MCommentD1002 = 25
1344         '
1345         EndIf
1346         '
1347     Next MStaNo
1348     '
1349     'Pias�֏����݊J�n M305 -> OfF
1350     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1351     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1352     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1353     '
1354     '
1355     '�ʉߗ���NG �H�������̏ꍇ
1356     If MJudge% = MPass% Then
1357         M_20# = MPass%
1358     EndIf
1359     '
1360    M_20# = MClear%     '������
1361     '
1362     '�G���[���
1363     If MJudge% < MOK% Then
1364     '
1365 '�c���Ă���������ł͎g�p���Ȃ����x��
1366 *RETRY_ERR_WRITE
1367         M_20# = MClear%     '������
1368         '�G���[�����L�q
1369         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1370         'GOT KEY���͑҂�
1371         MKeyNumber = fnKEY_WAIT()
1372         '
1373         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1374             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1375            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1376             Break
1377         '
1378         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1379             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1380             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1381             Break
1382         '
1383         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1384             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1385             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1386             Break
1387         '
1388         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1389             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1390            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1391             Break
1392         '
1393         EndIf
1394         '
1395 '        If M_20# = MClear% Then *RETRY_ERR_WRITE
1396         '
1397     EndIf
1398     '
1399     If M_20# = MContinue% Then *RETRY_PIASWRITE
1400     '
1401     fnPiasWrite = 1
1402     Exit Function
1403 FEnd
1404 '
1405 '��fnPCBNumberCheck
1406 ''' <summary>
1407 ''' Pias ��ԍ��ƍ��v��
1408 ''' </summary>
1409 '''<returns>0�i�Œ�j</returns>
1410 ''' <remarks>
1411 ''' Date   : 2021/07/07 : M.Hayakawa
1412 ''' </remarks>'
1413 Function M% fnPCBNumberCheck
1414       fnPCBNumberCheck = 0
1415     '
1416 *RETRY_PCBCHECK
1417     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1418     'Pias�֊�ƍ��J�n M310 -> ON
1419     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1420     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1421     '
1422     MJudge% = MNG%
1423     '
1424     For MStaNo = 0 To 5
1425         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1426             MJudge% = MOK%
1427             fnAutoScreenComment(96)  'AUTO���
1428             MStaNo = 5
1429             Break
1430         '
1431         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1432             MJudge% = MNG%
1433             fnAutoScreenComment(97)  'AUTO���
1434             MCommentD1001 = 37
1435             MCommentD1002 = 25
1436             MStaNo = 5
1437             Break
1438         '
1439         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1440             MJudge% = MNG%
1441             fnAutoScreenComment(98)  'AUTO���
1442             MCommentD1001 = 38
1443             MCommentD1002 = 25
1444             MStaNo = 5
1445             Break
1446         '
1447         ElseIf M_In(11580) = 1 Then                         'time out
1448             MJudge% = MNG%
1449             fnAutoScreenComment(99)  'AUTO���
1450             MCommentD1001 = 39
1451             MCommentD1002 = 25
1452             MStaNo = 5
1453             Break
1454         '
1455         Else
1456             MJudge% = MNG%
1457            MCommentD1001 = 41
1458            MCommentD1002 = 25
1459         '
1460         EndIf
1461         '
1462     Next MStaNo
1463     '
1464     'Pias�֊�ƍ��J�n M310 -> OfF
1465     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1466     '
1467     '
1468     '�ʉߗ���NG �H�������̏ꍇ
1469     If MJudge% = MPass% Then
1470         M_20# = MPass%
1471     EndIf
1472     '
1473    M_20# = MClear%     '������
1474     '
1475     '�G���[���
1476     If MJudge% < MOK% Then
1477     '
1478 '�c���Ă���������ł͎g�p���Ȃ����x��
1479 *RETRY_ERR_PCBNUMBER
1480         M_20# = MClear%     '������
1481         '�G���[�����L�q
1482         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1483         'GOT KEY���͑҂�
1484         MKeyNumber = fnKEY_WAIT()
1485         '
1486         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1487             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1488             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1489             Break
1490         '
1491         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1492             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1493             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1494         '
1495         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1496             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1497             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1498         '
1499         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1500             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1501             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1502             Break
1503         '
1504         EndIf
1505         '
1506 '        If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1507         '
1508     EndIf
1509     '
1510     If M_20# = MContinue% Then *RETRY_PCBCHECK
1511     Exit Function
1512 FEnd
1513 '
1514 '��ScrewTight
1515 ''' <summary>
1516 ''' �˂����߂��s��(S�^�C�g)
1517 ''' </summary>
1518 '''<param name="PScrewPos()">
1519 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1520 '''             PScrewPos(2)    �F�˂����߉��_
1521 '''             PScrewPos(10)   �F�˂����ߏI������
1522 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
1523 '''             1:6mm S�^�C�g��l�W
1524 '''             2:8mm P�^�C�g
1525 '''             3:6mm S�^�C�g���l�W
1526 '''             4:13mm S�^�C�g
1527 '''             5:6mm M�l�W
1528 '''</param>
1529 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
1530 '''<returns>����
1531 '''         0=�ُ�I���A1=����I���A�˂�����=-1
1532 '''</returns>
1533 ''' <remarks>
1534 ''' Date   : 2021/07/07 : M.Hayakawa
1535 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
1536 ''' Update : 2022/07/08 : ���� ���ŋz���m�F����悤�ɕύX
1537 ''' </remarks>'
1538 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
1539     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1540     ScrewTight = 0
1541     MOKNGFlg = 0
1542     Ovrd 100
1543     Mov PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
1544     MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1545     If MRtn = 0 Then
1546        ScrewTight = -1
1547     EndIf
1548     If MRtn = 0 Then GoTo *ScrewEnd
1549 '
1550     Fine 0.05 , P
1551     Ovrd MOvrdA%
1552     ' �����ݒ�
1553     Accel 100, 10
1554     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
1555     Mvs PScrewPosition(2)
1556     ' �����������ɖ߂�
1557     Accel
1558     ' ����Ovrd�ݒ�
1559 '    Ovrd MOvrdA%
1560     Ovrd 100
1561     ' Spd�ݒ�
1562 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1563     Spd MFeedSpd
1564     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
1565     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1566     Select MScrewType%
1567         Case 1
1568             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1569             ProgramBankSet(1,1)
1570             Break
1571         Case 2
1572             ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1573             ProgramBankSet(3,1)
1574             Break
1575         Case 3
1576             ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1577             ProgramBankSet(1,1)
1578             Break
1579         Case 4
1580             ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1581             ProgramBankSet(1,1)
1582             Break
1583         Case 5
1584             ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1585             ProgramBankSet(1,1)
1586             Break
1587         Case 6
1588             ' S�^�C�g�F�v���O����1�A�o���N4�ɐݒ�
1589             ProgramBankSet(1,4)
1590             Break
1591         Default
1592             ' �v���O����1�A�o���N�Ȃ��ݒ�
1593             ProgramBankSet(0,0)
1594             Break
1595     End Select
1596 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1597      '�h���C�o�[ON�@CW
1598     M_Out(12241)=1
1599     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1600     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
1601     Fine 0 , P
1602     Dly 0.1
1603     Spd M_NSpd
1604     '
1605     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
1606         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1607         Dly 0.1
1608        ' �v���O�����E�o���N����
1609         ProgramBankSet(0,0)
1610         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1611         Mvs PScrewPosition(10),-80
1612         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1613         M_Out(12249)=1 Dly 0.3
1614         MOKNGFlg = -1
1615         ScrewTight = 0
1616     Else
1617          '�h���C�o�[OFF�@CW
1618         M_Out(12241)=0
1619 '        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
1620 '        Select MScrewType%
1621 '            Case 1
1622 '                ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1623 '                ProgramBankSet(1,3)
1624 '                Break
1625 '            Case 2
1626 '                ' P�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1627 '                ProgramBankSet(3,3)
1628 '                Break
1629 '            Case 3
1630 '                ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
1631 '                ProgramBankSet(1,3)
1632 '                Break
1633 '            Case 4
1634 '                ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
1635 '                ProgramBankSet(1,3)
1636 '                Break
1637 '            Case 5
1638 '                ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
1639 '                ProgramBankSet(1,3)
1640 '                Break
1641 '            Default
1642 '                ' �v���O����1�A�o���N�Ȃ��ݒ�
1643 '                ProgramBankSet(0,0)
1644 '                Break
1645 '        End Select
1646 '         '�h���C�o�[ON�@CW
1647 '        Mvs PScrewPosition(10)
1648 '        M_Out(12241)=1
1649 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1650 '        Fine 0 , P
1651 '
1652          '�h���C�o�[OFF�@CW
1653         M_Out(12241)=0
1654        ' �v���O�����E�o���N����
1655         ProgramBankSet(0,0)
1656         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1657         M_Out(12249)=1 Dly 0.3
1658     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1659         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1660        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1661         'Mvs PScrewPosition(10),-80
1662         ScrewTight = 1
1663     EndIf
1664 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1665 '    Ovrd 10
1666 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1667     Ovrd 100
1668     Exit Function
1669 *ScrewEnd
1670 FEnd
1671 '
1672 '��ScrewGet
1673 ''' <summary>
1674 ''' �˂������@����˂��𓾂�
1675 ''' </summary>
1676 '''<param name="%">
1677 '''         PScrewPos(1)    �F�˂�������̂˂����
1678 '''         PScrewPos(2)    �F�˂���������_
1679 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1680 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1681 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1682 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1683 '''</param>
1684 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1685 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1686 '''<returns>����
1687 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1688 '''</returns>
1689 ''' <remarks>
1690 ''' Date   : 2021/07/07 : M.Hayakawa
1691 ''' </remarks>
1692 '''<update>
1693 '''Date    : 2021/11/15 : ����
1694 '''Date    : 2021/02/07 : ���� �O�̂��ߊm�F���폜
1695 '''</update>
1696 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1697     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
1698     ScrewGet = 0
1699     MScrewJudge% = 0
1700     '�˂������평������G���[�`�F�b�N
1701 ' ���b��폜
1702     'Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
1703     For MCnt% = 0 To MFinCnt%
1704         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1705         If MRtn = 0 Then
1706             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1707             ScrewGet = -1
1708             MScrewJudge% = 2
1709         EndIf
1710         Ovrd 100
1711         If FeederScrewSensor% <> 0 Then
1712             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1713                 'Ovrd 30
1714                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1715                 'NG�Ƃ��Ă����̊֐����甲����
1716                 ScrewGet = -2
1717                 MScrewJudge% = 3
1718             EndIf
1719         EndIf
1720         Ovrd 100
1721         Spd M_NSpd
1722         If MScrewJudge% = 0 Then
1723     '        ScrewGet = 0
1724             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1725             Dly 0.3
1726             MScrewCnt% = 0
1727             MFinCnt% = 2
1728             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1729             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1730             '
1731             '
1732             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1733             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1734             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1735             'Mvs PScrewPosition(10), 1.2
1736            Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
1737             '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX3/30����)
1738             M_Out(Y60_Driver)=1
1739             '�r�b�g��]�����ԊĎ��J�n
1740             M_Timer(4) = 0
1741             MloopFlg = 0
1742             MCrtTime& = 0
1743            '�r�b�g��]����܂őҋ@
1744             While MloopFlg = 0
1745                 MCrtTime& = M_Timer(4)
1746                 If MCrtTime& >= 180 Then
1747                     MloopFlg = 1
1748                 EndIf
1749             WEnd
1750             '
1751            M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1752             '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX3/30����)
1753 '            M_Out(Y60_Driver)=1
1754 '            Dly 0.2
1755             '�z���ʒu�ɂċz���m�F
1756             MRtn = 0
1757             MRtn = frInCheck(11264, 1, MSETTIMEOUT01&)      '�`�F�b�N�͂��邪�G���[����͂��Ȃ�
1758             '
1759             JOvrd M_NJovrd
1760             Spd M_NSpd
1761             '�l�W�z���m�F�ʒu�ړ�
1762             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1763             Mvs PScrewPosition(10), -30  ' �l�W�z���m�F�ʒu
1764            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1765             '�r�b�g��]��~
1766             M_Out(Y60_Driver)=0
1767             '
1768 '            If MRtn = 1 Then       '�ŏ���臒l�`�F�b�N���s��Ȃ�
1769                 '1�b�ԃl�W�z���m�F �n�߂�臒l
1770                 MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1771 '            EndIf
1772             'MRtn = 0'�����G���[
1773             '�z���G���[�̏ꍇ
1774             '�l�W���˂����Y�ɖ߂�
1775             If MRtn = 0 Then
1776                 Ovrd 30      '2����5�ɕύX'5����30�ɕύX(3/30����)
1777                 '�r�b�g��]��~
1778                 M_Out(Y60_Driver)=0
1779                 '�l�W�����@���
1780                 Mvs PScrewPosition(1)
1781                 '�X�ɏ��
1782                 Mov PScrewPosition(1), -140
1783                 '�l�W�̂Ĉʒu
1784                 If FeederReadyNo% = 11260 Then     '�����@�ʂɋz���G���[�����J�E���g�@2022/05/19 �n��
1785                     MRtn = FnCtlValue2(3)          '�����@�Q�z���G���[���{�P
1786                 Else
1787                     MRtn = FnCtlValue2(4)          '�����@�P�z���G���[���{�P  2022/04/28 �n��
1788                 EndIf
1789                 Mov PScrewPosition(9)
1790                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1791                 '�z��OFF
1792                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1793                 Dly 0.2
1794                 '�j��ON
1795                 M_Out(Y6B_VB1)=1 '�^��j��ON
1796                 '�r�b�g��]
1797                 M_Out(Y61_Driver)=1
1798                 Dly 0.5
1799                 '                '
1800                 Ovrd 100
1801                 JOvrd M_NJovrd
1802                 Spd M_NSpd
1803                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1804                 Mov PScrewPosition(9), 10
1805                 Mov PScrewPosition(9)
1806                 Dly 0.1
1807                 Mov PScrewPosition(9), 10
1808                 Mov PScrewPosition(9)
1809                 '
1810                 '�l�W�����҂�
1811                 Wait M_In(11265) = 0
1812                 '�r�b�g��]��~
1813                 M_Out(Y61_Driver)=0
1814                 Dly 0.1
1815                 '�j��OFF
1816                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1817                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1818                 Mov PScrewPosition(1), -140
1819                 Ovrd 100
1820                 Spd M_NSpd
1821                 '�l�W�����@���
1822                 Mvs PScrewPosition(1)
1823 '                '
1824                 ScrewGet = -3
1825                 If MCnt% = MFinCnt% Then
1826                     MScrewJudge% = 4
1827                     Mov PScrewPosition(2)
1828                     Break
1829                 EndIf
1830                 Break
1831 '                '
1832             Else
1833                 MCnt% = MFinCnt%
1834                 ScrewGet = 1
1835             EndIf
1836         Else
1837             MCnt% =MFinCnt%
1838         EndIf
1839     Next  MCnt%
1840         '
1841 '    If MScrewJudge% = 0 Then
1842 '        Ovrd 100
1843 '        Spd M_NSpd
1844 '        PScrewPosition(1)
1845 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1846 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1847 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1848 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1849 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1850 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1851 '        'Mov PScrewPosition(2)
1852 '        '������x�z���m�F�@���̍ŏI臒l
1853 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1854 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1855 '            MScrewJudge% = 4
1856 '            ScrewGet = -3
1857 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1858 '            MScrewJudge% = 1
1859 '            ScrewGet = 1
1860 '        EndIf
1861 '        Break
1862 '    EndIf
1863     '
1864 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1865     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1866     '
1867     Select MScrewJudge%
1868 '        Case 0
1869 ''            fErrorProcess(11,162,163,0) '�ُ�I��
1870 '            MCommentD1001 = 162
1871 '            MCommentD1002 = 96
1872 '            Break
1873         Case 2
1874 '            fErrorProcess(11,63,161,0) '����NG
1875             MCommentD1001 = 63
1876             MCommentD1002 = 96
1877             Break
1878         Case 3
1879 '            fErrorProcess(11,160,164,0) '�닟��
1880             MCommentD1001 = 237
1881             MCommentD1002 = 96
1882             Break
1883         Case 4
1884 '            fErrorProcess(11,94,95,0) '�z��NG
1885             MCommentD1001 = 94
1886             MCommentD1002 = 95
1887             Break
1888     End Select
1889     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1890     '
1891     Select M_20#
1892         Case MAbout%          '��~�������ꂽ�ꍇ
1893             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
1894             Mov PInitialPosition
1895             Break
1896         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1897             Break
1898         Case MNext%           '�p���������ꂽ�ꍇ
1899             M_20# = MClear%     '������
1900             Break
1901         Case MNgProcess%      'NG�������ꂽ�ꍇ
1902             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
1903             Mov PInitialPosition
1904             Break
1905         End Select
1906 *End_ScrewGet
1907     Exit Function
1908 FEnd
1909 '
1910 '��ProgramBankSet
1911 ''' <summary>
1912 ''' �˂����߂��s��(P�^�C�g)
1913 ''' </summary>
1914 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1915 '''<param name="MBankNo">�o���N�ԍ�</param>
1916 '''</returns>
1917 ''' <remarks>
1918 ''' Date   : 2021/10/05 : M.Hayakawa
1919 ''' </remarks>'
1920 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1921 '
1922     MLocalPrgNo% = (MProgramNo% - 1) * 32
1923     MLocalBankNo% = MBankNo% * 4
1924 '
1925     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1926         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1927     Else
1928         MLocalOutNo% = 0
1929     EndIf
1930 '
1931     M_Out8(12240) = MLocalOutNo%
1932     Dly 0.1
1933     Exit Function
1934 FEnd
1935 '
1936 '��fnKEY_WAIT()
1937 ''' <summary>
1938 ''' GOT����̃L�[���͑҂�
1939 ''' </summary>
1940 '''<returns>1�F��~    2�F����
1941 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1942 '''         5�FNG
1943 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1944 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1945 '''</returns>
1946 ''' <remarks>
1947 ''' Date   : 2021/07/07 : M.Hayakawa
1948 ''' </remarks>'
1949 Function M% fnKEY_WAIT()
1950     fnKEY_WAIT = 0
1951     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1952     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1953     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1954     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1955     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1956     Dly 0.2
1957     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1958     MLocalLoopFlg=1
1959     While MLocalLoopFlg=1
1960         If M_In(11345) = 1 Then         '��~   M5345
1961             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1962             fnKEY_WAIT = 1
1963             MLocalLoopFlg=-1
1964             Break
1965         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1966             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1967             fnKEY_WAIT = 2
1968             MLocalLoopFlg=-1
1969             Break
1970         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1971             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1972             fnKEY_WAIT = 3
1973             MLocalLoopFlg=-1
1974             Break
1975         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1976             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1977             fnKEY_WAIT = 4
1978             MLocalLoopFlg=-1
1979             Break
1980         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1981             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1982             fnKEY_WAIT = 5
1983             MLocalLoopFlg=-1
1984             Break
1985         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1986             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1987             fnKEY_WAIT = MRobotInit1%
1988             MLocalLoopFlg=-1
1989             Break
1990         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1991             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1992             fnKEY_WAIT = MRobotInit2%
1993             MLocalLoopFlg=-1
1994             Break
1995         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1996             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1997             fnKEY_WAIT = MRobotInit3%
1998             MLocalLoopFlg=-1
1999             Break
2000         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2001             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2002             fnKEY_WAIT = MRobotInit4%
2003             MLocalLoopFlg=-1
2004             Break
2005         Else
2006         EndIf
2007     WEnd
2008     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2009     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2010     Exit Function
2011 FEnd
2012 '
2013 '�� fnAUTO_CTL
2014 ''' <summary>
2015 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2016 ''' </summary>
2017 ''' <remarks>
2018 ''' Date   : 2021/07/07 : M.Hayakawa
2019 ''' </remarks>
2020 Function M% fnAUTO_CTL
2021     fnAUTO_CTL = 0
2022     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2023     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2024     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2025     '
2026     If M_Svo=0 Then             '�T�[�{ON�m�F
2027         Servo On
2028     EndIf
2029     Wait M_Svo=1
2030     Exit Function
2031 FEnd
2032 '
2033 '�� fnWindScreenOpen
2034 ''' <summary>
2035 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2036 ''' </summary>
2037 '''<param name="%"></param>
2038 '''<param name="%"></param>
2039 '''<param name="%"></param>
2040 '''<param name="%"></param>
2041 ''' <remarks>
2042 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2043 ''' MWindReSet = 0     ��ʔ�\��
2044 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2045 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2046 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2047 ''' Date   : 2021/07/07 : M.Hayakawa
2048 ''' </remarks>
2049 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2050     If MCommentD1001 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2051         M_Out16(12480) = MCommentD1001      'D1001 �R�����g
2052     EndIf
2053     '
2054     If MCommentD1002 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2055         M_Out16(12496) = MCommentD1002      'D1002 �R�����g
2056     EndIf
2057     '
2058     If MCommentD1003 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2059        M_Out16(12512) = MCommentD1003       'D1003 �R�����g
2060     EndIf
2061     '
2062     M_Out16(12448) = MScreenNo              '��ʔԍ�  M6448   10=�G���[���
2063     M_Out(12363) = 1 Dly 0.5                '�E�B���h��ʐݒ�  M6362
2064     Exit Function
2065 FEnd
2066 '
2067 '��FnCtlValue2
2068 ''' <summary>
2069 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2070 ''' </summary>
2071 ''' <param name="MCtlNo%"></param>
2072 ''' <remarks>
2073 ''' Date : 2022/04/28 �n��
2074 ''' </remarks>
2075 '''
2076 '''  1�F������       �{�P
2077 '''  2�F�g���n�j��   �{�P
2078 '''  3�F�����@�Q�z���G���[�� �{�P�@�@�g��NG����ύX 2022/05/19 �n��
2079 '''  4�F�����@�P�z���G���[�� �{�P
2080 ''' 99�F�Ǐ��J�n�M�� OFF
2081 '''
2082 Function M% FnCtlValue2(ByVal MCtlNo%)
2083     FnCtlValue2 = 1
2084     Select MCtlNo%
2085         Case 1        '�������{�P
2086             M_Out(12569) = 0             '�����݊J�n�M��OFF
2087             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2088             MInputQty = M_In16(11600)    '��������M
2089             MInputQty = MInputQty + 1    '�������{�P
2090             M_Out16(12592) = MInputQty   '���������M
2091             M_Out(12569) = 1             '�����݊J�n�M��ON
2092             Break
2093             '
2094         Case 2        '�g���n�j���{�P
2095             M_Out(12569) = 0             '�����݊J�n�M��OFF
2096             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2097             MAssyOkQty = M_In16(11616)   '�g��OK����M
2098             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2099             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2100             M_Out(12569) = 1             '�����݊J�n�M��ON
2101             Break
2102             '
2103         Case 3        '�����@�Q�z���G���[���{�P
2104             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2105             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2106             MSuctionErrQty = M_In16(11632)         '�����@�Q�z���G���[����M
2107             MSuctionErrQty = MSuctionErrQty + 1    '�����@�Q�z���G���[���{�P
2108             M_Out16(12624) = MSuctionErrQty        '�����@�Q�z���G���[�����M
2109             M_Out(12569) = 1                       '�����݊J�n�M��ON
2110             Break
2111             '
2112         Case 4        '�����@�P�z���G���[���{�P
2113             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2114             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2115             MSuctionErrQty = M_In16(11648)         '�����@�P�z���G���[����M
2116             MSuctionErrQty = MSuctionErrQty + 1    '�����@�P�z���G���[���{�P
2117             M_Out16(12640) = MSuctionErrQty        '�����@�P�z���G���[�����M
2118             M_Out(12569) = 1                       '�����݊J�n�M��ON
2119             Break
2120             '
2121         Case 99        '�Ǐ��J�n�M��OFF
2122             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2123             M_Out(12569) = 0        '�����݊J�n�M��OFF
2124             Break
2125             '
2126     End Select
2127     Exit Function
2128 FEnd
2129 '
2130 '
2131 '��FnScreEroorCord
2132 ''' �d���h���C�o�[�̃G���[�R�[�h���܂߂��R�����g���o���ׂ̃R�����g�ԍ��̍쐬
2133 ''' �V�K�쐬�F2022/05/23 : �n��
2134 '''
2135 Function M% FnScreEroorCord()
2136     MScrewErrorCord% = 0
2137     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2138     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2139     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2140     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2141     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2142     MScrewErrorCord% = MScrewErrorCord% * 10
2143     MScrewErrorCord% = MScrewErrorCord% + 500
2144     FnScreEroorCord = MScrewErrorCord%
2145     Exit Function
2146 FEnd
2147 '
2148 '
2149 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2150 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2151 '-------------------------------------------------------------------------------
2152 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2153 '   ����
2154 '       PInspPos()      �F�����ʒu
2155 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2156 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2157 '       MInspCnt%       �F�����ʒu��
2158 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2159 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2160 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2161 '   �߂�l�F����
2162 '       0=�ُ�I���A1=����I��
2163 '
2164 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2165 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2166 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2167 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2168 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2169 '-------------------------------------------------------------------------------
2170     '----- �����ݒ� -----
2171     Cnt 0                                                           '�ړ�����������(�����l=0)
2172     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2173 '    Cnt 1,0.1,0.1
2174     '�ϐ��錾�E������
2175     Def Inte MNum                                                   '�����ԍ�(������1�`)
2176     MNum% = 1                                                       '�����ԍ������l�ݒ�
2177     Def Inte MEndFlg                                                '�����I���t���O
2178     MEndFlg% = 0
2179     '
2180     '����G�ԍ��ݒ�v���E�������s�v��off
2181     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2182     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2183     '�G���[�ԍ��N���A
2184     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2185     M_Out16(MOUT_InspErrNum) = MInspErrNum
2186     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2187     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2188     '
2189     'Insight Ready check?
2190     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2191         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2192         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2193         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2194         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2195         'Exit Function
2196     EndIf
2197     If MInspErrNum = 20 Then GoTo *ISInspectionSingle_End
2198     '
2199     '�����ʒu���m�F
2200     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2201         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2202         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2203         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2204         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2205         'Exit Function
2206     EndIf
2207    If MInspErrNum = 21 Then GoTo *ISInspectionSingle_End
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
2228             Mov PInspPos( MNum% )                                       '�ړ�
2229             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2230             Dly 0.2                                                     '�ړ�������Delay 0.05>>0.2
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
2386 '
2387 *ISInspectionSingle_End
2388     Exit Function
2389 FEnd
2390 '
2391 '��fnAutoScreenComment
2392 ''' <summary>
2393 ''' ���C����ʂ̓���󋵕\��
2394 ''' �R�����gD1005�̐ݒ�
2395 ''' </summary>
2396 '''<param name="McommentD1005%">�R�����gID</param>
2397 ''' <remarks>
2398 ''' Date   : 2021/07/07 : M.Hayakawa
2399 ''' </remarks>
2400 Function fnAutoScreenComment(ByVal McommentD1005%)
2401     M_Out16(12576) = McommentD1005%
2402     Exit Function
2403 FEnd
2404 '
2405 '��fnRoboPosChk
2406 ''' <summary>
2407 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2408 ''' </summary>
2409 '''<param name="MINNumber%">���͔ԍ�</param>
2410 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2411 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2412 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2413 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2414 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2415 ''' <remarks>
2416 ''' Date   : 2021/07/07 : M.Hayakawa
2417 ''' </remarks>
2418 Function M% fnRoboPosChk
2419     fnRoboPosChk = 0
2420     MRet = fnStepRead()
2421     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2422     '�E�B���h��ʐ؊���
2423     If MRBTOpeGroupNo > 5 Then
2424         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2425         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2426         Dly 0.2
2427         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2428         Dly 1.5
2429         '
2430         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2431         '
2432         MLoopFlg% = 1
2433         While MLoopFlg% = 1
2434             '
2435             '
2436             MKeyNumber% = fnKEY_WAIT()
2437             Select MKeyNumber%
2438                 Case Is = MAbout%       '��~
2439                     M_20# = MAbout%
2440                     MLoopFlg% = -1
2441                     Break
2442                 Case Is = MNext%        '����
2443                     'MLoopFlg% = -1
2444                     Break
2445                 Case Is = MContinue%    '�p��
2446                     M_20# = MContinue%
2447                     MLoopFlg% = -1
2448                     Break
2449                 Default
2450                     Break
2451             End Select
2452         WEnd
2453     EndIf
2454     '
2455     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2456         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2457         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2458         Select MRBTOpeGroupNo
2459             Case Is = 5                          '�������Ȃ�
2460                 Break
2461             Case Is = 10                         '�����ʒu�֖߂�
2462                 'Mov PTEST001
2463                 Break
2464             Case Is = 15                         '�����ʒu�֖߂�
2465                 'Mov PTEST002
2466                 Dly 0.5
2467                 'Mov PTEST001
2468                 Dly 0.5
2469                 Break
2470             Default
2471                 Break
2472         End Select
2473         '
2474         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2475         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2476         MRBTOpeGroupNo = 5
2477         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2478         Dly 1.0
2479         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2480         fnRoboPosChk = 1                        '�����ʒu������s
2481         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2482     EndIf
2483     Exit Function
2484 FEnd
2485 '
2486 '��frInCheck
2487 ''' <summary>
2488 ''' �Z���T�[IN�`�F�b�N
2489 ''' </summary>
2490 '''<param name="MINNumber%">���͔ԍ�</param>
2491 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2492 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2493 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2494 ''' <remarks>
2495 ''' Date   : 2021/07/07 : M.Hayakawa
2496 ''' </remarks>
2497 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2498     M_Timer(4) = 0
2499     MloopFlg = 0
2500     While MloopFlg = 0
2501         MCrtTime& = M_Timer(4)
2502         If M_In(MINNumber%) = MCMPFLG% Then
2503             MloopFlg = 1
2504             frInCheck = 1
2505         ElseIf MCrtTime& > MTimeCnt& Then
2506             MloopFlg = 1
2507             frInCheck = 0
2508         EndIf
2509     WEnd
2510     Exit Function
2511 FEnd
2512 '-----------------------------------------------
2513 '
2514 '�˂����ߋ@�ʐM�m�F
2515 '
2516 '-----------------------------------------------
2517 Function M% fScewTcomChk
2518     fScewTcomChk = 0
2519     '�ʐM�m�F���M
2520     M_Out(MOUT_ScwT_ComChk%) = MOn%
2521     '�ʐM�m�F��M�ҋ@
2522     Wait M_In(MIN_ScwT_comOK%) = MOn%
2523     '�ʐM�m�F���M�I��
2524     M_Out(MOUT_ScwT_ComChk%) = MOff%
2525     Exit Function
2526 FEnd
2527 '
2528 '
2529 '-----------------------------------------------
2530 '
2531 '�˂����ߊJ�n���M
2532 '
2533 '-----------------------------------------------
2534 Function M% fScewTStart
2535     fScewTStart = 0
2536     '�˂����ߊJ�n�ҋ@����M
2537     Wait M_In(MIN_ScwT_STRec%) = MOn%
2538     Dly 0.1
2539     '�˂����ߊJ�n��M�𑗐M
2540     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
2541     Exit Function
2542 FEnd
2543 '
2544 '
2545 '-----------------------------------------------
2546 '
2547 '�˂����ߊ�����M
2548 '
2549 '-----------------------------------------------
2550 Function M% fScewTFinish
2551     fScewTFinish = 0
2552     '�˂����ߊ����ҋ@����M
2553     Wait M_In(MIN_ScwT_Fin%) = MOn%
2554     Dly 0.1
2555     '�˂����ߊ�����M�𑗐M
2556     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
2557     Exit Function
2558 FEnd
2559 '
2560 '
2561 '-----------------------------------------------
2562 '
2563 '����xx��~��M
2564 '
2565 '-----------------------------------------------
2566 Function M% fScewTCaseStop(ByVal MCase%())
2567     fScewTCaseStop = 0
2568     '����xx��~����M
2569     Wait M_In(MCase%(1)) = MOn%
2570     Dly 0.1
2571     '����xx��~��M�𑗐M
2572     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
2573     Exit Function
2574 FEnd
2575 '
2576 '-----------------------------------------------
2577 '
2578 '�ĊJ�n��M
2579 '
2580 '-----------------------------------------------
2581 Function M% fScewTReStart()
2582     fScewTReStart = 0
2583     '�ĊJ�n����M
2584     Wait M_In(MIN_ScwT_ReST%) = MOn%
2585     Dly 0.1
2586     '�ĊJ�n��M�𑗐M
2587     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
2588     Exit Function
2589 FEnd
2590 '
2591 '��fErrorProcess
2592 '<summary>
2593 '�G���[����
2594 '</summary>
2595 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2596 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2597 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2598 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2599 '<make>
2600 '2021/11/5 �����V��
2601 '</make>
2602 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2603     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2604     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2605     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2606     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2607 *RETRY_ERR_PROCESS
2608      M_20# = MClear%     '������
2609 '        '�G���[�����L�q
2610         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2611 '        'GOT KEY���͑҂�
2612         MKeyNumber = fnKEY_WAIT()
2613 '        '
2614         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2615             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2616  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2617             Break
2618          '
2619         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2620             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2621  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2622         '
2623         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2624             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2625  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2626          '
2627         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2628             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2629  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2630             Break
2631         '
2632         EndIf
2633         '
2634         '
2635         '
2636         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2637         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2638     Exit Function
2639 FEnd
2640 '
2641 '��fnInitialZone
2642 ''' <summary>
2643 ''' ���݈ʒu������ɑҔ����A�����ʒu�ɖ߂�
2644 ''' </summary>
2645 ''' <remarks>
2646 ''' Date : 2021/12/2 : M.Hayakawa
2647 ''' Update:2022/06/2 : M.Hayakawa ���H���̔���~���A�ɍ��킹�ĕύX
2648 ''' </remarks>
2649 Function fnInitialZone()
2650     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���]
2651 '
2652     Ovrd 5
2653 ' ���ޔ�
2654     PActive = P_Curr
2655     Pmove = PActive
2656 '
2657     If PActive.X > 580 Then
2658         Pmove.Z =380        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2659     Else
2660         Pmove.Z =500        '��L�ȊO��Z:500�܂Ŏ����グ
2661     EndIf
2662 '
2663     Mvs Pmove
2664     Mov PInitialPosition
2665 ' ���b�N���J��
2666     InitialState()
2667 ' ��U��~
2668     fErrorProcess(20,70,256,0)
2669     Exit Function
2670 FEnd
2671 '
2672 '��InitialState
2673 ''' <summary>
2674 ''' �n���h�A����������ʒu�ɂ���
2675 ''' </summary>
2676 ''' <returns>   0 : OK
2677 '''             1 : NG
2678 ''' </returns>
2679 ''' <remarks>
2680 ''' Date : 2021/12/2 : M.Hayakawa
2681 ''' </remarks>
2682 Function M% InitialState()
2683     InitialState = 0
2684     '���i�ʒu���߉���
2685     M_Out(12261)=1 Dly 0.3      'FAN�N�����v�o�[�p���X�o��
2686     'Wait M_In(11271)=1          'FAN�N�����v�o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2687     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'FAN�N�����v�o�[���o(8/26����)
2688     If MRtn = 0 Then
2689         fErrorProcess(11,234,284,0)
2690         Select M_20#
2691             Case MAbout%            '��~��
2692                 InitialState = 1
2693                 Break
2694             Case MNgProcess%        'NG�������ꂽ�ꍇ
2695                 InitialState = 0
2696                 Break
2697             Case MContinue%
2698                 M_20# = MClear%
2699                 InitialState = 0
2700                 Break
2701             Case MNext%
2702                 M_20# = MClear%
2703                 InitialState = 0
2704                 Break
2705         End Select
2706     EndIf
2707     '
2708     M_Out(12259)=1 Dly 0.3      '�v�b�V��CY�pSV�ߒ[�p���X�o��
2709     'Wait M_In(11269)=1          '�v�b�V���ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2710     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    '�v�b�V���ߒ[���o
2711     If MRtn = 0 Then
2712         fErrorProcess(11,234,284,0)
2713         Select M_20#
2714             Case MAbout%            '��~��
2715                 InitialState = 1
2716                 Break
2717             Case MNgProcess%        'NG�������ꂽ�ꍇ
2718                 InitialState = 1
2719                 Break
2720             Case MContinue%
2721                 M_20# = MClear%
2722                 InitialState = 0
2723                 Break
2724             Case MNext%
2725                 M_20# = MClear%
2726                 InitialState = 0
2727                 Break
2728         End Select
2729     EndIf
2730     '
2731     M_Out(12257)=1 Dly 0.3      '�ʒu����CY�pSV�ߒ[�p���X�o��
2732     'Wait M_In(11267)=1          '�ʒu���ߖߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2733     MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[���o(8/26����)
2734     If MRtn = 0 Then
2735         fErrorProcess(11,234,284,0)
2736         Select M_20#
2737             Case MAbout%            '��~��
2738                 InitialState = 1
2739                 Break
2740             Case MNgProcess%        'NG�������ꂽ�ꍇ
2741                 InitialState = 1
2742                 Break
2743             Case MContinue%
2744                 M_20# = MClear%
2745                 InitialState = 0
2746                 Break
2747             Case MNext%
2748                 M_20# = MClear%
2749                 InitialState = 0
2750                 Break
2751         End Select
2752     EndIf
2753     Exit Function
2754 FEnd
2755 '
2756 '��fnTorqueCheck
2757 ''' <summary>
2758 ''' �g���N�`�F�b�N����p�̃��C��
2759 ''' </summary>
2760 ''' <remarks>
2761 ''' Date   : 2021/12/21 : H.AJI
2762 ''' </remarks>'
2763 Function M% fnTorqueCheck
2764     '�g���N�`�F�b�N�����M  �����n��~
2765     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2766     '
2767     fnTorqueCheck = 0
2768     Ovrd 20
2769     Mov PInitialPosition              '�����ʒu�ړ�
2770     Ovrd 100
2771     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2772     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2773     Dly 0.2
2774     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2775     '
2776     'M6340  �g���N�`�F�b�N��M
2777     M_Out(12340) = 1 Dly 1.0                '�g���N�`�F�b�N��M M6340
2778     Dly 1.0
2779     M_Out(12340) = 0
2780     '
2781     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
2782     '
2783     MLoopFlg = 1
2784     While MLoopFlg = 1
2785         '
2786         Mov PInitialPosition              '�����ʒu�ړ�
2787         '
2788         MKeyNumber = fnKEY_WAIT()
2789         Select MKeyNumber
2790             Case Is = 1           '��~
2791                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
2792                 Dly 1.0
2793                 M_Out(12343) = 0
2794                 Ovrd 20
2795                 Mov PTicketRead_1
2796                 Ovrd 100
2797                 M_20# = 1
2798                 MLoopFlg = -1
2799                 Break
2800             Case Is = 2           '����
2801                 Break
2802             Case Is = 3           '�p��
2803                 Break
2804             Case Is = 4           '�g���N�`�F�b�N�J�n
2805                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
2806                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
2807                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2808                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2809                 MRet = fnMoveTorquePosi()
2810                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
2811                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2812                 Break
2813             Default
2814                 Break
2815         End Select
2816     WEnd
2817     '
2818     '�g���N�`�F�b�N����~���M
2819     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2820     '
2821     '���{�b�g�̈ʒu�����ɖ߂�
2822     '
2823     Exit Function
2824  FEnd
2825  '
2826 '
2827 '
2828 '---------------------------
2829 '
2830 '    ���C����ʂ̕\���A��\���ݒ�
2831 '         �R�����gD1001, D1002, D1003�̐ݒ�
2832 '           MWindReSet = 0     ��ʔ�\��
2833 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2834 '           MWindErrScr = 10    �G���[��� D1001, D1002
2835 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2836 '
2837 '---------------------------
2838 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2839     fnMainScreenOpen = 0
2840     '
2841    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2842         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2843     EndIf
2844     '
2845     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2846         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2847     EndIf
2848     '
2849     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2850         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2851     EndIf
2852     '
2853     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2854     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
2855     Dly 0.5
2856     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
2857     Exit Function
2858 FEnd
2859 '
2860 '��Main
2861 ''' <summary>
2862 ''' �g���N�`�F�b�N������
2863 ''' </summary>
2864 ''' <remarks>
2865 ''' Date   : 2021/12/21 : H.AJI
2866 ''' </remarks>'
2867 Function M% fnMoveTorquePosi
2868      fnMoveTorquePosi = 0
2869      Ovrd 50
2870      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
2871     '
2872     Spd M_NSpd
2873 '-------------      �h���C�o�[RST
2874     M_Out(12240)=0     '�h���C�o�[OFF CCW
2875     M_Out(12241)=0     '�h���C�o�[OFF CW
2876     M_Out(12242)=1     '�h���C�o�[���� C1
2877     M_Out(12243)=1     '�h���C�o�[���� C2
2878     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
2879 '---------------------------------------
2880 '[P-11]
2881 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
2882     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
2883     Dly 0.1
2884 '-----------------------
2885    'Cnt 0                           'Cnt����-2�@�I��
2886 '-----------------------
2887     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
2888     Dly 0.2
2889 '-----------------------
2890     ProgramBankSet(1,3)
2891     M_Out(12241)=0                   '�h���C�o�[OFF  CW
2892     'Dly 0.1
2893 '--------------------------------
2894     Ovrd 40
2895    'Dly 0.1
2896 '--------------------------------  �l�W���ߑ��x�ݒ�
2897     Spd 14                            '���C�h 100-40 100% :Spd 12
2898     Dly 0.1
2899 '--------------------------------
2900 '--------------------------------
2901 '---------------------------------�y�˂����ߓ���z
2902 '
2903     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2904    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2905     Dly 0.3                          '�������҂�
2906    M_Out(12241)=1                   '�h���C�o�[ON  CW
2907 '
2908     Wait M_In(11584)=1                '����/�G���[���o
2909     Dly 0.1
2910     Spd M_NSpd
2911    'Ovrd 20
2912     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2913     Wait M_In(11257)=1                '�l�W����SC
2914 '---------------------------------
2915     Dly 0.1
2916     M_Out(12241)=0                    '�h���C�o�[OFF CW
2917     Dly 0.1
2918     M_Out(12242)=0                    '�h���C�o�[���� C1
2919     Dly 0.1
2920     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2921     Dly 0.1
2922     M_Out(12245)=0                    '�v���O����2���� F1
2923 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2924 '
2925     Mvs PTorqueCheck,-60                       '������mov ����ύX
2926     Dly 0.1
2927 '--------------------------------------------------------------
2928    'Ovrd 80
2929 '--------------------------------------------------------------
2930 '---------------------------------------
2931 '---------------------------------------
2932 '---------------------------------------�G���[���E����
2933    *LBL1
2934    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2935    Mvs ,-100
2936    M_Out(12241)=0     '�h���C�o�[OFF CW
2937    Dly 0.1
2938    M_Out(12242)=0     '�h���C�o�[���� C1
2939    Dly 0.1
2940    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2941    Dly 0.1
2942    M_Out(12245)=0     '�v���O�������� F1
2943 '---------------------------------------
2944 '---------------------------------------
2945 '-------------
2946    'Mov PInitPos19049
2947    Dly 0.1
2948 '
2949 '
2950     Exit Function
2951 FEnd
2952 '
2953 '��Main
2954 ''' <summary>
2955 ''' �g������p�̃��C��
2956 ''' </summary>
2957 ''' <remarks>
2958 ''' Date   : 2021/07/07 : M.Hayakawa
2959 ''' </remarks>'
2960 Function Main
2961     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2962     '
2963     If M_Svo=0 Then
2964         Servo On
2965     EndIf
2966     Wait M_Svo=1
2967 '�g���X�^�[�g���t�����v���p���XON
2968     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2969 '�p�g���C�g����
2970     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2971     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2972     '
2973     M_20# = 0                                   'KEY���͏�����
2974     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
2975     MRet% = 0
2976 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2977     PActive = P_Curr                    '���݈ʒu���擾
2978     MRecoveryPass% = 0
2979     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2980         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2981             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2982             MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2983         EndIf
2984     EndIf
2985     EndIf
2986     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2987         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2988             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2989                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2990             EndIf
2991         EndIf
2992     EndIf
2993     If MRecoveryPass% = 0 Then
2994         fnInitialZone()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2995     EndIf
2996 '
2997     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2998         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2999 '�g���N�`�F�b�N
3000         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3001             MRet% = fnTorqueCheck()
3002             Break
3003         Else
3004 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3005 '                MRtn = InspInit()               '�摜��������������
3006 '            EndIf
3007             '
3008            M_20# = MClear%                    '������
3009 '�g���J�n
3010             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3011                 MRet% = fnAssyStart()
3012             Else
3013                 M_20# = MPass%
3014             EndIf
3015 '�g���I�����t����
3016             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3017             Wait M_In(11572) = 1            '���t�擾����
3018             Dly 0.1
3019             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3020             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3021             fnAutoScreenComment(89)         'AUTO��� �g����������
3022 ' ��H���փt���O�o��
3023             If M_20# <> MAbout% Then
3024                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3025             ElseIf M_20# = MPass% Then
3026                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3027             EndIf
3028 'About(��~)�ȊO��OK���o�́i�p���b�g�~���j
3029             If M_20# <> MAbout% Then
3030                 M_Out(12339) = 1 Dly 0.5    'M6339  toPLC_RBT�����p���X�o��
3031             EndIf
3032             M_Out(12346) = 0                ' M6346 toPLC_�g���J�n��M OFF
3033 'PIAS�ɑg������������
3034             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3035                 If M_20# = MPass% Then
3036                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3037                 Else
3038                     'KEY���͂�NG�̏ꍇ
3039                     If M_20# = MNgProcess% Then
3040 '                        M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3041                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3042                         MRet% = fnPiasWrite(MNG%)
3043                        nAssyNgQty = nAssyNgQty + 1
3044                     EndIf
3045                     '
3046                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3047                     If M_20# = MAssyOK% Then
3048                             '-----------------------
3049                             'D732 -> D2600 �R�s�[�v��
3050                             M_Out(12566) = 1
3051 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3052                             M_Out(12566) = 0
3053                             '
3054                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3055                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3056                             '��ԍ��ƍ�(PP�͖��g�p�j
3057 '                            MRet% = fnPCBNumberCheck()
3058                         Else
3059                             MRet% = 1
3060                         EndIf
3061                         '
3062                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3063                             If M_20# <> MAbout% Then
3064                                 '�H������OK��������
3065                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3066                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3067                                 MRet% = fnPiasWrite(MOK%)
3068                                 nAssyOkQty = 0
3069                                 nAssyOkQty = nAssyOkQty + 1
3070                             Else
3071                                 nAssyOkQty = nAssyOkQty + 1
3072                             EndIf
3073                         EndIf
3074                     EndIf
3075 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3076 '                    MRet% = fnPiasWrite(MOK%)
3077                 EndIf
3078             Else
3079                 nAssyOkQty = nAssyOkQty + 1
3080             EndIf
3081             '
3082             '�g���I�����t��������
3083             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3084             '�������A�g��OK���A�g��NG��������
3085 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3086             '
3087 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3088 '                '�摜�����I������
3089 '                MRtn = InspQuit()
3090 '            EndIf
3091         EndIf
3092         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3093     EndIf
3094 '�p�g���C�g����
3095     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3096     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3097 'GOT�\��
3098     fnAutoScreenComment(93)  'AUTO��� �H������
3099 '    M_Out(12339) = 1 Dly 0.5        ' M6339 toPLC_RBT�����p���XON
3100 '    M_Out(12346) = 0        'M6346  toPLC_AssY�J�n��M OFF
3101 '
3102 FEnd
3103 End
3104 '
3105 '
3106 '���܂��Ȃ��R�����g
3107 '��΍폜�����
3108 '
3109 '
3110 '
3111 '
PInspPosition(1)=(+313.20,-30.00,+430.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+348.35,+127.39,+410.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(3)=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PInspPosition(4)=(+520.00,-46.00,+397.00,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
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
PTemp=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(1)=(+349.00,+91.40,+370.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(2)=(+349.00,+91.40,+313.84,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPosTemp(10)=(+349.00,+91.40,+303.84,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(1)=(+233.44,+389.39,+380.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(2)=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05,+0.00,+0.00)(7,0)
PGetScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(9)=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(10)=(+233.44,+389.39,+338.70,-180.00,+0.00,+179.99,+0.00,+0.00)(7,0)
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
PActive=(+602.00,-150.75,+450.00,-180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
Pmove=(+300.00,+0.00,+500.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PCalcGetMainScrew=(+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PCalcGetFanScrew=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGyroPcbRead=(+315.06,-65.41,+419.00,+160.00,+0.00,+90.00)(7,0)
PGyroPcbRead_1=(+329.62,-57.71,+450.00,-180.00,+0.00,+90.00)(7,0)
PInitialPosition=(+300.00,+0.00,+450.00,-180.00,+0.00,-180.00)(7,0)
PMainPcbRead=(+309.58,-174.17,+408.00,-180.00,+0.00,+180.00)(7,0)
PMainPcbRead_1=(+309.58,-174.17,+450.00,-180.00,+0.00,+180.00)(7,0)
PParts1Check=(+313.20,-30.00,+430.00,+180.00,+0.00,-180.00)(7,0)
PParts1Check_1=(+313.20,-30.00,+480.00,-180.00,+0.00,+180.00)(7,0)
PParts2Check=(+348.35,+127.39,+410.00,+180.00,+0.00,-180.00)(7,0)
PParts2Check_1=(+348.35,+127.39,+450.00,+180.00,+0.00,+180.00)(7,0)
PParts3Check=(+538.88,+54.12,+420.00,+180.00,+0.00,-180.00)(7,0)
PParts3Check_1=(+538.88,+54.12,+460.00,-180.00,+0.00,+180.00)(7,0)
PParts4Check=(+520.00,-46.00,+397.00,-180.00,+0.00,+180.00)(7,0)
PParts4Check_1=(+520.00,-46.00,+460.00,-180.00,+0.00,-180.00)(7,0)
PScrewFan1=(+317.47,+123.55,+303.88,+180.00,+0.00,+90.00)(7,0)
PScrewFan1_0=(+317.47,+123.55,+313.88,+180.00,+0.00,+90.00)(7,0)
PScrewFan1_1=(+317.47,+123.55,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewFan2=(+349.00,+91.40,+303.84,+180.00,+0.00,+90.00)(7,0)
PScrewFan2_0=(+349.00,+91.40,+313.84,+180.00,+0.00,+90.00)(7,0)
PScrewFan2_1=(+349.00,+91.40,+370.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain1=(+305.10,-26.35,+304.11,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_0=(+305.10,-26.35,+310.11,-180.00,+0.00,+90.00)(7,0)
PScrewMain1_1=(+305.10,-26.35,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain2=(+305.00,-175.00,+304.33,-180.00,+0.00,+90.00)(7,0)
PScrewMain2_0=(+305.00,-175.00,+310.33,+180.00,+0.00,+90.00)(7,0)
PScrewMain2_1=(+305.00,-175.00,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain3=(+366.84,-180.48,+304.36,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_0=(+366.84,-180.48,+310.36,-180.00,+0.00,+90.00)(7,0)
PScrewMain3_1=(+366.84,-180.48,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain4=(+379.32,+24.31,+305.67,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_0=(+379.32,+24.31,+311.67,-180.00,+0.00,+60.00)(7,0)
PScrewMain4_1=(+379.32,+24.31,+380.00,-180.00,+0.00,+60.00)(7,0)
PScrewMain5=(+385.18,-105.04,+323.66,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_0=(+385.18,-105.04,+335.06,-180.00,+0.00,+90.00)(7,0)
PScrewMain5_1=(+385.18,-105.04,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewMain6=(+325.37,-78.92,+323.77,+180.00,+0.00,+90.00)(7,0)
PScrewMain6_0=(+325.37,-78.92,+334.17,-180.00,+0.00,+90.00)(7,0)
PScrewMain6_1=(+325.37,-78.92,+380.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupplyFan=(+233.52,+389.28,+338.64,-180.00,+0.00,+179.99)(7,0)
PScrewSupplyFan_1=(+233.52,+389.28,+380.00,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyFan_2=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyFan_9=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyMain=(+103.30,+195.32,+338.34,-180.00,+0.00,-180.00)(7,0)
PScrewSupplyMain_1=(+103.30,+195.32,+380.00,-180.00,+0.00,+180.00)(7,0)
PScrewSupplyMain_2=(+166.05,+146.93,+447.34,-180.00,+0.00,+128.05)(7,0)
PScrewSupplyMain_9=(-3.19,+216.64,+432.44,+180.00,+0.00,-180.00)(7,0)
PTicketRead=(+602.00,-150.75,+373.00,+180.00,-0.02,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.75,+450.00,+180.00,-0.02,+90.00)(7,0)
PTorqueCheck=(+143.66,-242.00,+340.00,-180.00,-0.01,+90.00)(7,0)
PTorqueCheck_1=(+143.66,-242.00,+360.00,-180.00,-0.01,+90.00)(7,0)
