1 ' ===================================
2 '
3 '  210543200 STEP5 Assy6�v���O����CD�p
4 '
5 ' �쐬�ҁF������T
6 ' �쐬���F2022.07.16
7 ' Ver 00 2021.07.16
8 '
9 ' Ver 00 2022.07.16 FA3 DVD�p���x�[�X��FA2�Ɠ��l�̕ύX
10 ' ===================================
11 '===== <Insight�萔> =====
12 '===== <Insight�ϐ���`> =====
13 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
14 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
15 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
16 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
17 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
18 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
19 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
20 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
21 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
22 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
23 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
24 '
25 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
26 'Output Signal
27 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
28 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
29 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
30 '
31 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
32 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
33 '
34 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
35 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
36 Def Inte MOUT_OKNG                 '
37 '��Ɨp�ϐ�
38 Def Inte MInspErrNum                '�������s�G���[�ԍ�
39 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
40 Def Inte MRtn                       'Function�߂�l�擾�p
41 Def Inte MRtn2                      'Function�߂�l�擾�p
42 Def Inte MRet3                      'Function�߂�l�擾�p
43 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
44 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
45 Def Inte MovrdA                     '�l�W����Ovrd �ϗp
46 Def Float MSpdA                     '�l�W����Spd�@�ϗp
47 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
48 Def Inte MKeyNum                    'KEY���͎󂯎��p(�ǉ�12/20����)
49 '===== <Insight�ϐ��ݒ�> =====
50 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
51 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
52 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
53 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
54 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
55 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
56 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
57 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
58 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
59 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
60 'Output Signal
61 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
62 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
63 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
64 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
65 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
66 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
67 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
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
89 X34_ScrewReady1=11259 '�˂�����1�@Read
90 '===== <�d�h���萔> =====
91 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
92 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
93 Dim PEscapePosi(10)
94 MLoopCnt% = 0'
95 '===== <���{�b�g�萔> =====
96 '===== <���{�b�g�ϐ���`> =====
97 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
98 MCommentD1001 = 0
99 MCommentD1002 = 0
100 MCommentD1003 = 0
101 MScreenNo = 0
102 '
103 MCommentTSU = 0
104 MCommentTSD = 0
105 '�E�B���h��ʔԍ��ݒ�
106 MWindReSet = 0
107 MWindInfoScr = 5
108 MWindErrScr = 10
109 MWindErrScr2 = 11
110 MWindErrScr3 = 13
111 MWindErrScr17 = 17
112 MWindErrScr18 = 18
113 MWindCmmnScr = 20
114 MWindJigRelase19049 = 60
115 MWindJigRelase19050 = 61
116 MWindJigRelase19051 = 62
117 '
118 MClear% = 0        'KEY_�̃N���A
119 MAbout% = 1        'KEY_��~
120 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
121 MContinue% = 3     'KEY_�p�� �ēx����������s��
122 '
123 MKeyNum% = 0       'KEY���͂��󂯎��
124 '
125 Def Inte MNgProcess
126 MNgProcess% = 5      'KEY_NG
127 '
128 MAssyOK% = 6       '�g������
129 MPass% = 7         '�H���p�X
130 MPiasNG% = 8       'Pias�m�F������NG
131 MIrregular% = 10   '��O�������s�p
132 MPiasOK% = 11      'PIAS��ǂ�OK��
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
150 MOK% = 1               '�e����p
151 MNG% = 0               '�e����p
152 MTIMEOUT% = -1         '�e����p
153 MJudge% = 0            '������i�[�p
154 '
155 MRECIVETIME& = 0
156 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
157 MSETTIMEOUT08& = 8000&                 ' 8�b�ݒ�
158 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
159 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
160 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
161 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
162 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
163 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
164 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
165 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
166 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
167 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
168 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
169 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
170 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
171 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
172 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
173 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
174 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
175 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
176 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
177 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
178 MIN_PIAS_MyProcessComp% = 11573        '���H����������
179 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
180 MOUT_OKNG% = 12226                     'PLC OUT ��OK=1, NG=0 �o��
181 '
182 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
183 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
184 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
185 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
186 '
187 MOUT_PiasAssyResultOK% = 12549    '�g��OK
188 MOUT_PiasAssyResultNG% = 12550    '�g��NG
189 MOUT_PiasAssyResultWr% = 12548    '�H��������������
190 '
191 MIN_PiasProcessNG% = 11559        '�H����������NG
192 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
193 MIN_PiasProcessOK% = 11558        '�H����������OK
194 '
195 MIN_Insight_Use% = 11374               '�摜�m�FON
196 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
197 '
198 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
199 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
200 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
201 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
202 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
203 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
204 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
205 '
206 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
207 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
208 '
209 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
210 '
211 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
212 '
213 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
214 MopeNo% = 0
215 MRtn% = 0
216 MRet = 0
217 MRet3% = 0
218 '
219 Def Inte MInputQty          '������ ���Z�ϐ�
220 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
221 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
222 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
223 Def Inte nAssyOkQty         '���g�p
224 Def Inte MScrewNo
225 Def Inte MReTry
226 '===== <IO�ϐ���`> =====
227 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
228 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
229 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
230 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
231 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
232 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
233 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
234 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
235 '
236 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
237 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
238 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
239 '
240 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
241 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
242 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
243 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
244 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
245 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
246 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
247 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
248 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
249 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
250 '
251 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
252 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
253 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
254 '
255 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
256 '
257 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
258 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
259 '
260 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
261 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
262 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
263 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
264 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
265 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
266 '
267 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
268 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
269 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
270 '
271 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
272 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
273 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
274 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
275 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
276 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
277 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
278 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
279 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
280 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
281 '
282 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
283 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
284 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
285 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
286 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
287 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
288 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
289 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
290 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
291 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
292 '
293 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
294 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
295 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
296 '
297 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
298 '
299 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
300 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
301 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
302 '
303 '����
304 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
305 Def Inte MOn                            '�o��=1
306 Def Inte MOff                           '�o��=0
307 '
308 '�˂����ߑ��u_�o�̓A�h���X
309 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
310 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
311 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
312 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
313 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
314 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
315 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
316 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
317 '�˂����ߑ��u_���̓A�h���X
318 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
319 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
320 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
321 Def Inte MIN_ScwT_Case1                 '����1��~����M
322 Def Inte MIN_ScwT_Case2                 '����2��~����M
323 Def Inte MIN_ScwT_Case3                 '����3��~����M
324 Def Inte MIN_ScwT_Case4                 '����4��~����M
325 Def Inte MIN_ScwT_Case5                 '����5��~����M
326 '
327 Dim MScwT_Case1%(2)               '����1��~�ϐ�
328 Dim MScwT_Case2%(2)               '����2��~�ϐ�
329 Dim MScwT_Case3%(2)               '����3��~�ϐ�
330 Dim MScwT_Case4%(2)               '����4��~�ϐ�
331 Dim MScwT_Case5%(2)               '����5��~�ϐ�
332 '
333 '����
334 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
335 MOn% = 1                                 '�o�� = 1
336 MOff% = 0                                '�o�� = 0
337 '
338 '�˂����ߋ@_�A�h���X�ݒ�
339 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
340 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
341 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
342 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
343 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
344 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
345 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
346 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
347 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
348 '
349 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
350 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
351 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
352 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
353 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
354 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
355 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
356 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
357 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
358 '
359 MScwT_Case1%(1) = MIN_ScwT_Case1%
360 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
361 MScwT_Case2%(1) = MIN_ScwT_Case2%
362 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
363 MScwT_Case3%(1) = MIN_ScwT_Case3%
364 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
365 MScwT_Case4%(1) = MIN_ScwT_Case4%
366 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
367 MScwT_Case5%(1) = MIN_ScwT_Case5%
368 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
369 '
370 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
371 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
372 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
373 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
374 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
375 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
376 Def Inte MRecoveryPass         '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
377 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
378 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
379 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
380 '
381 '
382 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
383 Function M% fnAssyStart
384     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
385 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
386     M_20# = MClear%                       '������
387 '    If M_22# = MIrregular% Then GoTo *IRREGULAR     'Assy������DVD���J��c�����Ă���ꍇ
388     '�J�n�ʒu���C�j�V�����|�W�V�����������ꍇ(�n���h��������������`�F�b�N)
389     *RE_START
390     PTemp = P_Curr
391     MRtn = 0
392     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
393         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
394             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
395                 MRtn = 1
396             EndIf
397         EndIf
398     EndIf
399     '�C�j�V�����|�W�V�����ɂ����ꍇ�Z���T�[�`�F�b�N
400     If MRtn = 1 Then
401         Ovrd 20
402         Accel 100 , 20
403         Mvs PHandChange                             '�n���h�����ʒu�Ɉړ�
404         Ovrd 100
405         Accel 100 , 100
406         MRtn = frInCheck(11264,0,MSETTIMEOUT03&)    '�Z���T�[�`�F�b�N(���������)
407         Mvs PInitialPosition                        '�C�j�V�����|�W�V�����Ɉړ�
408     Else
409         MRtn = 1
410     EndIf
411     If MRtn = 1 Then GoTo *AssyStart
412     fErrorProcess(11,286,287,0)                        '�n���h�����p�������
413     If M_20# = MNext% Then M_20# = MClear%
414     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
415     If M_20# = MNgProcess% Then GoTo *RE_START
416     If M_20# = MContinue% Then GoTo *RE_START
417 '
418     *AssyStart
419 '�g�ݗ��ĊJ�n(���g��9/10����)
420 '�v���O�������_
421 Ovrd 100
422 '�n���h��DVD���J,�u���P�b�g��������
423 '
424 *INITIAL_CHECK
425 '
426 If M_In(11264) = 0 And M_In(11267) = 0 And M_In(11270) = 0 Then GoTo *CompInitial1  'DVD���J,�u���P�b�g��������
427 fErrorProcess(11,253,287,0)                 '284��287�ɕύX6/2����
428 If M_20# = MNext% Then M_20# = MClear%
429 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
430 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
431 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
432 *CompInitial1
433 '
434 '�n���h���C�j�V�����ɖ߂�
435 If M_In(11266) = 1 Then     'DVD�`���b�N���o
436     M_Out(12256) = 0        'DVD�`���b�N��OFF
437     M_Out(12257) = 1        'DVD�`���b�N�JON
438     Break
439 EndIf
440 If M_In(11269) = 1 Then     'F�V�����_�[�o���o
441     M_Out(12258) = 0        'F�V�����_�[�oOFF
442     M_Out(12259) = 1        'F�V�����_�[��ON
443     Break
444 EndIf
445 If M_In(11272) = 1 Then     'R�V�����_�[�o���o
446     M_Out(12260) = 0        'R�V�����_�[�oOFF
447     M_Out(12261) = 1        'R�V�����_�[��ON
448     Break
449 EndIf
450 '
451 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'DVD�`���b�N�J���o
452 If MRtn = 1 Then GoTo *CompInitial2
453 fErrorProcess(11,270,284,0)
454 If M_20# = MNext% Then M_20# = MClear%
455 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
456 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
457 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
458 *CompInitial2
459 '
460 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)    'F�V�����_�[�ߌ��o
461 If MRtn = 1 Then GoTo *CompInitial3
462 fErrorProcess(11,278,284,0)
463 If M_20# = MNext% Then M_20# = MClear%
464 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
467 *CompInitial3
468 '
469 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    'R�V�����_�[�ߌ��o
470 If MRtn = 1 Then GoTo *CompInitial4
471 fErrorProcess(11,276,284,0)
472 If M_20# = MNext% Then M_20# = MClear%
473 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
474 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
475 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
476 *CompInitial4
477 '
478 ' 2022/04/11 ���S�����֏����ǉ� �n��
479 ' PInitialPosition �ݐ� MStandby=1
480 ' PMechaOnJigGet_3 �ݐ� MStandby=2
481 '
482 MStandby = 0    '�ҋ@�ʒu�t���O��������
483 PTemp = P_Curr
484 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
485     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
486         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
487             MStandby = 1
488         EndIf
489     EndIf
490 EndIf
491 If (PTemp.X <= PMechaOnJigGet_3.X + 1.0) And (PTemp.X >= PMechaOnJigGet_3.X - 1.0) Then
492     If ((PTemp.Y <= PMechaOnJigGet_3.Y + 1.0) And (PTemp.Y >= PMechaOnJigGet_3.Y - 1.0)) Then
493         If ((PTemp.Z <= PMechaOnJigGet_3.Z + 1.0) And (PTemp.Z >= PMechaOnJigGet_3.Z - 1.0)) Then
494             MStandby = 2
495         EndIf
496     EndIf
497 EndIf
498 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
499     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
500         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
501             MStandby = 3
502         EndIf
503     EndIf
504 EndIf
505 If MStandby <> 0 Then GoTo *PositionOK
506 fErrorProcess(11,230,281,0)          '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
507 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
508 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
509 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
510 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
511 *PositionOK
512 '
513 '
514 'DVD����PASS�v���O��������ʏ�v���O�����ɐؑւ������̑΍� 2022.05.13 �n��
515 PTemp = P_Curr
516 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
517     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
518         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
519             Ovrd 50
520             Mov PInitialPosition
521             Ovrd 100
522         EndIf
523     EndIf
524 EndIf
525 '
526 ' CD�ɂ��ȍ~�̑g������폜 2022/07/16 M.H
527 MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
528 MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
529 M_20# = MAssyOK%
530 '
531 *ASSY_ERROR_END
532 *AssyEnd
533 *fnAssyStart_FEndPosi
534 FEnd
535 '
536 '��fnPiasCheck
537 ''' <summary>
538 ''' PIAS�`�P�b�g�Ǎ���
539 ''' </summary>
540 ''' <returns>   0 : NG
541 '''             1 : OK(�Ǎ��݊���)
542 ''' </returns>
543 ''' <remarks>
544 ''' Date   : 2021/07/07 : M.Hayakawa
545 ''' </remarks>'
546 Function M% fnPiasCheck
547     fnPiasCheck = 0
548     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
549     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
550 '
551 *RETRY_PIAS
552     M_20# = MClear%
553     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
554     '
555     '�yID�`�P�b�g�ǂݍ��݁z
556     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
557     MInspGroup%(1) = 1              '����G�ԍ�
558     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
559 '
560     '�G���[�̏ꍇ
561     If MRtn <> 1 Then
562         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
563         If MRtn <> 1 Then
564             'D720 -> D1300 �R�s�[�v��
565             M_Out(12565) = 1
566             Dly 0.5
567             M_Out(12565) = 0
568             '�G���[�����L�q
569             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
570             'GOT KEY���͑҂�
571             MKeyNumber = fnKEY_WAIT()
572         '
573             Select MKeyNumber
574                 Case MNext%         '���ւ�I�������ꍇ
575                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
576                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
577 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
578                     Break
579                 Case MAbout%        '��~��I�������ꍇ
580                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
581                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
582 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
583                     Break
584                 Case MNgProcess%    'NG��I�������ꍇ
585                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
586                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
587 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
588                     Break
589                 Case MContinue%     '�p����I�������ꍇ
590                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
591                     M_20# = MContinue%
592 '                    GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C(�֐��O���ŕ��򂷂�悤�ύX3/26����)
593                     Break
594             End Select
595         EndIf
596     EndIf
597     If M_20# <> MClear% Then GoTo *fnPiasCheck_End
598 '
599 '----------D720 -> D1300 �R�s�[�v��----------
600     M_Out(12565) = 1
601     Dly 0.5
602     M_Out(12565) = 0
603 '----------�ʐM�m�F������----------
604     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
605     MRtn = 0                ' ������
606     M_20# = MClear%         ' ������
607     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
608     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j(�֐��O���ŕ��򂷂�悤�ɕύX3/26����)
609     If MRtn <> 1 Then GoTo *fnPiasCheck_End
610 '        If M_20# = MContinue% Then
611 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
612 '        Else
613 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
614 '        EndIf
615 '    EndIf
616 '----------�H�������m�F----------
617     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
618     MRtn = 0                ' ������
619     M_20# = MClear%         ' ������
620     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
621     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j(�֐��O���ŕ��򂷂�悤�ɕύX3/26����)
622     If MRtn <> 1 Then GoTo *fnPiasCheck_End
623 '        If M_20# = MContinue% Then
624 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
625 '        Else
626 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
627 '        EndIf
628 '    EndIf
629     '
630     fnPiasCheck = 1
631     *fnPiasCheck_End
632 FEnd
633 '
634 '��fnPCComuCheck
635 ''' <summary>
636 ''' PC-PLC�ʐM�`�F�b�N
637 ''' </summary>
638 ''' <returns>   0 : NG
639 '''             1 : OK(�Ǎ��݊���)
640 ''' </returns>
641 ''' <remarks>
642 ''' Date   : 2021/07/07 : M.Hayakawa
643 ''' </remarks>'
644 Function M% fnPCComuCheck
645     fnPCComuCheck = 0
646     MJudge% = 0                                  '������
647     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
648     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
649     '
650     For MStaNo = 0 To 5
651         '
652         If M_In(MIN_PIAS_ComOK%) = 1 Then
653             'PC�ʐMOK(M400)
654             MJudge% = MOK%
655             MStaNo = 5
656             Break
657         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
658             'toRBT_�ʐM�m�Ftime out
659             MJudge% = MNG%
660             MCommentD1001 = 15
661             MCommentD1002 = 21
662             MStaNo = 5
663             Break
664         Else
665             'toRBT_�ʐM�m�Ftime out
666             MJudge% = MNG%
667             MCommentD1001 = 14
668             MCommentD1002 = 21
669             Break
670         EndIf
671     Next MStaNo
672     '
673     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
674     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
675     '
676     '�G���[���
677     If MJudge% <> MOK% Then
678         M_20# = MClear%     '������
679         '�G���[�����L�q
680         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
681         'GOT KEY���͑҂�
682         MKeyNumber = fnKEY_WAIT()
683         '
684         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
685             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
686             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
687             Break
688         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
689             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
690             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
691             Break
692         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
693             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
694             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
695             Break
696         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
697             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
698             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
699             Break
700         EndIf
701     Else
702         'OK�̏ꍇ
703         fnPCComuCheck = 1
704     EndIf
705 FEnd
706 '
707 '��fnProcessCheck
708 ''' <summary>
709 ''' �H�������m�F
710 ''' </summary>
711 ''' <returns>    1�F�H������OK     0�F�ُ�I��
712 '''             -1�F�O�H������NG  -2�F���H����������
713 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
714 '''             -5�F���������G���[
715 ''' </returns>
716 ''' <remarks>
717 ''' Date   : 2021/07/07 : M.Hayakawa
718 ''' </remarks>'
719 Function M% fnProcessCheck
720     fnProcessCheck = 0
721     MJudge% = MNG%      '��UNG���������Ƃ���
722 '----------�H�������m�F----------
723     MCommentD1001 = 0   '�R�����g������
724     For MStaNo = 0 To 5
725         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
726         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
727         '
728         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
729             MJudge% = MOK%
730             fnAutoScreenComment(85)     ' AUTO���
731             MStaNo = 5
732             Break
733         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
734             MFlgLoop% = 0
735             MJudge% = MNG%
736             MCommentD1001 = 27
737             MCommentD1002 = 22
738             fnAutoScreenComment(94)     ' AUTO���
739             fnProcessCheck = -2         ' NG��-2��Ԃ�
740             MStaNo = 5
741             Break
742         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
743            MJudge% = MNG%
744             MCommentD1001 = 31
745             MCommentD1002 = 22
746             fnAutoScreenComment(83)     ' AUTO���
747             fnProcessCheck = -3         ' NG��-3��Ԃ�
748             MStaNo = 5
749             Break
750         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
751             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
752             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
753             MJudge% = MNG%
754             MCommentD1001 = 32
755             MCommentD1002 = 22
756             fnAutoScreenComment(84)     ' AUTO���
757             fnProcessCheck = -1         ' NG��-1��Ԃ�
758             Dly 1.0
759             '�H�������m�FOFF
760             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
761             Dly 1.0
762            'MStaNo = 5
763             Break
764         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
765             MFlgLoop% = 0
766             MJudge% = MNG%
767             MCommentD1001 = 29
768             MCommentD1002 = 22
769             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
770             fnProcessCheck = -5         ' NG��-5��Ԃ�
771             MStaNo = 5
772             Break
773         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
774             MJudge% = MNG%
775             If MCommentD1001 = 32 Then
776                 '�������Ȃ�
777             Else
778                 MCommentD1001 = 26
779             EndIf
780             MCommentD1002 = 22
781             fnProcessCheck = -4         ' NG��-4��Ԃ�
782             MStaNo = 5
783             Break
784         Else
785             MJudge% = MNG%
786             MCommentD1001 = 28
787             MCommentD1002 = 22
788         EndIf
789     Next MStaNo
790     '�H�������m�FOFF
791     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
792     '�ʉߗ���NG �H�������̏ꍇ
793     If MJudge% = MPass% Then
794         M_20# = MPass%
795     EndIf
796     '
797     '�G���[���
798     If MJudge% <> MOK% Then
799         M_20# = MClear%     '������
800         '�G���[�����L�q
801         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
802         'GOT KEY���͑҂�
803         MKeyNumber = fnKEY_WAIT()
804         '
805         Select MKeyNumber
806             Case MAbout%        '��~��I�������ꍇ
807                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
808                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
809                 Break
810             Case MNext%         '���ւ�I�������ꍇ
811                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
812                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
813                 Break
814             Case MContinue%     '�p����I�������ꍇ
815                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
816                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
817                 Break
818             Case MNgProcess%    'NG��I�������ꍇ
819                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
820                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
821                 Break
822         End Select
823     Else
824         fnProcessCheck = 1  ' OK��1��Ԃ�
825     EndIf
826 FEnd
827 '
828 '��fnPiasWrite
829 ''' <summary>
830 ''' Pias �g�����ʏ����ݗv��
831 ''' </summary>
832 '''<param name="MFlg%">
833 '''                 MOK%(1) = �H��������OK��������
834 '''                 MNG%(0) = �H��������NG��������
835 '''</param>
836 '''<returns></returns>
837 ''' <remarks>
838 ''' Date   : 2021/07/07 : M.Hayakawa
839 ''' </remarks>'
840 Function M% fnPiasWrite(ByVal MFlg%)
841       fnPiasWrite = 0
842 *RETRY_PIASWRITE
843     '
844     '�g��OK(MOK%)�̏ꍇ�@M306 ON
845    '�g��NG(MNG%)�̏ꍇ�@M307 ON
846     If MFlg% = MOK% Then
847         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
848     Else
849         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
850     EndIf
851     Dly 0.1                  '�O�̂���
852     '
853     'Pias�֏����݊J�n M305 -> ON
854     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
855     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
856     '
857     MJudge% = MNG%
858     '
859     For MStaNo = 0 To 5
860         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
861             MJudge% = MOK%
862             'MRet = fnAutoScreenComment(85)  'AUTO���
863             MStaNo = 5
864             Break
865         '
866         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
867             MJudge% = MNG%
868             'MRet = fnAutoScreenComment(85)  'AUTO���
869            MCommentD1001 = 34
870            MCommentD1002 = 25
871             MStaNo = 5
872             Break
873         '
874         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
875             MJudge% = MNG%
876             'MRet = fnAutoScreenComment(85)  'AUTO���
877            MCommentD1001 = 35
878            MCommentD1002 = 25
879             MStaNo = 5
880             Break
881         '
882         ElseIf M_In(11583) = 1 Then                         '�H����������time out
883             MJudge% = MNG%
884             'MRet = fnAutoScreenComment(85)  'AUTO���
885            MCommentD1001 = 36
886            MCommentD1002 = 25
887             MStaNo = 5
888             Break
889         '
890         Else
891             MJudge% = MNG%
892            MCommentD1001 = 42
893            MCommentD1002 = 25
894         '
895         EndIf
896         '
897     Next MStaNo
898     '
899     'Pias�֏����݊J�n M305 -> OfF
900     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
901     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
902     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
903     '
904     '
905     '�ʉߗ���NG �H�������̏ꍇ
906     If MJudge% = MPass% Then
907         M_20# = MPass%
908     EndIf
909     '
910    M_20# = MClear%     '������
911     '
912     '�G���[���
913     If MJudge% < MOK% Then
914     '
915 '�c���Ă���������ł͎g�p���Ȃ����x��
916 *RETRY_ERR_WRITE
917         M_20# = MClear%     '������
918         '�G���[�����L�q
919         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
920         'GOT KEY���͑҂�
921         MKeyNumber = fnKEY_WAIT()
922         '
923         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
924             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
925            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
926             Break
927         '
928         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
929             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
930             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
931         '
932         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
933             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
934             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
935         '
936         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
937             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
938            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
939             Break
940         '
941         EndIf
942         '
943         If M_20# = MClear% Then *RETRY_ERR_WRITE
944         '
945     EndIf
946     '
947     If M_20# = MContinue% Then *RETRY_PIASWRITE
948     '
949     fnPiasWrite = 1
950     '
951 FEnd
952 '
953 '��fnPCBNumberCheck ��ԍ��ƍ��̂Ȃ��H���ɂ��폜 2022/07/16
954 '
955 '��ScrewTight_S2 �˂����߂Ȃ��H���ɂ��폜 2022/07/16 M.H
956 '
957 '��ScrewGet_S3 �˂����߂Ȃ��H���ɂ��폜 2022/07/16 M.H
958 '
959 '��fnKEY_WAIT()
960 ''' <summary>
961 ''' GOT����̃L�[���͑҂�
962 ''' </summary>
963 '''<returns>1�F��~    2�F����
964 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
965 '''         5�FNG
966 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
967 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
968 '''</returns>
969 ''' <remarks>
970 ''' Date   : 2021/07/07 : M.Hayakawa
971 ''' </remarks>'
972 Function M% fnKEY_WAIT()
973     fnKEY_WAIT = 0
974     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
975     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
976     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
977     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
978     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
979     Dly 0.2
980     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
981     MLocalLoopFlg=1
982     While MLocalLoopFlg=1
983         If M_In(11345) = 1 Then         '��~   M5345
984             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
985             fnKEY_WAIT = 1
986             MLocalLoopFlg=-1
987             Break
988         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
989             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
990             fnKEY_WAIT = 2
991             MLocalLoopFlg=-1
992             Break
993         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
994             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
995             fnKEY_WAIT = 3
996             MLocalLoopFlg=-1
997             Break
998         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
999             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1000             fnKEY_WAIT = 4
1001             MLocalLoopFlg=-1
1002             Break
1003         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1004             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1005             fnKEY_WAIT = 5
1006             MLocalLoopFlg=-1
1007             Break
1008             '
1009         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1010             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1011             fnKEY_WAIT = MRobotInit1%
1012             MLocalLoopFlg=-1
1013             Break
1014             '
1015         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1016             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1017             fnKEY_WAIT = MRobotInit2%
1018             MLocalLoopFlg=-1
1019             Break
1020             '
1021         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1022             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1023             fnKEY_WAIT = MRobotInit3%
1024             MLocalLoopFlg=-1
1025             Break
1026             '
1027         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1028             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1029             fnKEY_WAIT = MRobotInit4%
1030             MLocalLoopFlg=-1
1031             Break
1032             '
1033         Else
1034         EndIf
1035     WEnd
1036     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1037     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
1038 FEnd
1039 '
1040 '�� fnAUTO_CTL
1041 ''' <summary>
1042 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
1043 ''' </summary>
1044 ''' <remarks>
1045 ''' Date   : 2021/07/07 : M.Hayakawa
1046 ''' </remarks>
1047 Function M% fnAUTO_CTL
1048     fnAUTO_CTL = 0
1049     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1050     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
1051     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1052     '
1053     If M_Svo=0 Then             '�T�[�{ON�m�F
1054         Servo On
1055     EndIf
1056     Wait M_Svo=1
1057 FEnd
1058 '
1059 '�� fnWindScreenOpen
1060 ''' <summary>
1061 ''' �E�B���h��ʂ̕\���A��\���ݒ�
1062 ''' </summary>
1063 '''<param name="%"></param>
1064 '''<param name="%"></param>
1065 '''<param name="%"></param>
1066 '''<param name="%"></param>
1067 ''' <remarks>
1068 ''' �R�����gD1001, D1002, D1003�̐ݒ�
1069 ''' MWindReSet = 0     ��ʔ�\��
1070 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
1071 ''' MWindErrScr = 10    �G���[��� D1001, D1002
1072 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
1073 ''' Date   : 2021/07/07 : M.Hayakawa
1074 ''' </remarks>
1075 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1076     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1077         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
1078     EndIf
1079     '
1080     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1081         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
1082     EndIf
1083     '
1084     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1085        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
1086     EndIf
1087     '
1088     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
1089     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
1090     Dly 0.5
1091     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
1092 FEnd
1093 '
1094 '��FnCtlValue2
1095 ''' <summary>
1096 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
1097 ''' </summary>
1098 ''' <param name="MCtlNo%"></param>
1099 ''' <remarks>
1100 ''' Date : 2022/04/28 �n��
1101 ''' </remarks>
1102 '''
1103 '''  1�F������       �{�P
1104 '''  2�F�g���n�j��   �{�P
1105 '''  3�F�g���m�f��   �{�P (���g�p)
1106 '''  4�F�z���G���[�� �{�P
1107 ''' 99�F�Ǐ��J�n�M�� OFF
1108 '''
1109 Function M% FnCtlValue2(ByVal MCtlNo%)
1110     FnCtlValue2 = 1
1111     Select MCtlNo%
1112         Case 1        '�������{�P
1113             M_Out(12569) = 0             '�����݊J�n�M��OFF
1114             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
1115             MInputQty = M_In16(11600)    '��������M
1116             MInputQty = MInputQty + 1    '�������{�P
1117             M_Out16(12592) = MInputQty   '���������M
1118             M_Out(12569) = 1             '�����݊J�n�M��ON
1119             Break
1120             '
1121         Case 2        '�g���n�j���{�P
1122             M_Out(12569) = 0             '�����݊J�n�M��OFF
1123             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
1124             MAssyOkQty = M_In16(11616)   '�g��OK����M
1125             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
1126             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
1127             M_Out(12569) = 1             '�����݊J�n�M��ON
1128             Break
1129             '
1130         Case 4        '�z���G���[���{�P
1131             M_Out(12569) = 0                       '�����݊J�n�M��OFF
1132             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
1133             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
1134             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
1135             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
1136             M_Out(12569) = 1                       '�����݊J�n�M��ON
1137             Break
1138             '
1139         Case 99        '�Ǐ��J�n�M��OFF
1140             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
1141             M_Out(12569) = 0        '�����݊J�n�M��OFF
1142             Break
1143             '
1144     End Select
1145     Exit Function
1146 FEnd
1147 '
1148 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
1149 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
1150 '-------------------------------------------------------------------------------
1151 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
1152 '   ����
1153 '       PInspPos()      �F�����ʒu
1154 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
1155 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
1156 '       MInspCnt%       �F�����ʒu��
1157 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
1158 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
1159 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
1160 '   �߂�l�F����
1161 '       0=�ُ�I���A1=����I��
1162 '
1163 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
1164 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
1165 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
1166 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
1167 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
1168 '-------------------------------------------------------------------------------
1169     '----- �����ݒ� -----
1170     Cnt 0                                                           '�ړ�����������(�����l=0)
1171     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
1172 '    Cnt 1,0.1,0.1
1173     '�ϐ��錾�E������
1174     Def Inte MNum                                                   '�����ԍ�(������1�`)
1175     MNum% = 1                                                       '�����ԍ������l�ݒ�
1176     Def Inte MEndFlg                                                '�����I���t���O
1177     MEndFlg% = 0
1178     '
1179     '����G�ԍ��ݒ�v���E�������s�v��off
1180     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
1181     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
1182     '�G���[�ԍ��N���A
1183     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
1184     M_Out16(MOUT_InspErrNum) = MInspErrNum
1185     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
1186     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
1187     '
1188     'Insight Ready check?
1189     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
1190         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
1191         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
1192         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
1193         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
1194         Exit Function
1195     EndIf
1196     '
1197     '�����ʒu���m�F
1198     If MInspCnt% < 1 Or 30 < MInspCnt% Then
1199         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
1200         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
1201         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
1202         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
1203         Exit Function
1204     EndIf
1205     '
1206     '
1207     '
1208     '----- ���C������ -----
1209     '�ݒ肳�ꂽ�����ʒu�����̌������s
1210     While( MEndFlg% = 0 )
1211         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
1212         MSetGrNumRetryExitFlg = 0
1213         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
1214         While( MSetGrNumRetryExitFlg = 0 )
1215         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
1216             '
1217             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
1218             '
1219             '----- �����O���[�v�ԍ��ݒ� -----
1220             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
1221             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
1222             '
1223             '�����ʒu�ֈړ��E�ړ������҂�
1224             Mvs PInspPos( MNum% )                                       '�ړ�
1225             Dly 0.05                                                    '�ړ�������Delay
1226             '
1227             '�����O���[�v�ԍ��ݒ�I���m�F
1228             M_Timer(1) = 0
1229             MExitFlg = 0
1230             While( MExitFlg = 0 )
1231                 '����G�ݒ萳��I��?
1232                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
1233                     MExitFlg = 1
1234                 '
1235                 '����G�ݒ�ُ�I��?
1236                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
1237                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
1238                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
1239                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
1240                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
1241                     EndIf
1242                     MExitFlg = 1
1243                 '
1244                 'timeout�`�F�b�N
1245                 ElseIf 1000 < M_Timer(1) Then
1246                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
1247                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
1248                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
1249                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
1250                     EndIf
1251                     MExitFlg = 1
1252                 EndIf
1253             WEnd
1254             '
1255             '����G�ԍ��ݒ�v��off
1256             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
1257             '
1258             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
1259             'NG�Ȃ���Δ�����
1260             If MCurrentStepErr = 0 Then
1261                 MSetGrNumRetryExitFlg = 1
1262             Else
1263                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
1264                 If MSetGrNumRetryCnt = 0 Then
1265                     MSetGrNumRetryExitFlg = 1
1266                 Else
1267                     'Retry�ց@���̑O��Delay
1268                     Dly 0.5
1269                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
1270                 EndIf
1271             EndIf
1272             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
1273             '
1274         WEnd
1275         '
1276         '
1277         '
1278         '----- �������s -----
1279         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
1280             If 0 < MInspGrNum%(MNum%) Then                          '��������?
1281                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
1282                 MInspRetryExitFlg = 0
1283                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
1284                 While( MInspRetryExitFlg = 0 )
1285                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
1286                     '
1287                     '���������m�F
1288                     MRetryCnt = MRetryCnt - 1
1289                     M_Timer(1) = 0
1290                     MExitFlg = 0
1291                     While( MExitFlg = 0 )
1292                     '���������҂�
1293                         '����OK�I��?
1294                         If M_In( MIN_IS_InspOK% ) = 1  Then
1295                             MJudgeOKFlg = 1                         '����OK�t���OON
1296                             MExitFlg = 1
1297                         '
1298                         '����NG�I��?
1299                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
1300                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
1301                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
1302                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
1303                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
1304                                 EndIf
1305                             EndIf
1306                             MExitFlg = 1
1307                         '
1308                         '�����ُ�I��(IS timeout)?
1309                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
1310                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
1311                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
1312                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
1313                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
1314                                 EndIf
1315                             EndIf
1316                             MExitFlg = 1
1317                         '
1318                         'timeout�`�F�b�N
1319                         ElseIf 3000 < M_Timer(1) Then
1320                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
1321                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
1322                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
1323                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
1324                                 EndIf
1325                             EndIf
1326                             MExitFlg = 1
1327                         EndIf
1328                     WEnd
1329                     '
1330                     '�����J�n�v��off
1331                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
1332                     '
1333                     'OK�Ȃ甲����
1334                     If MJudgeOKFlg = 1 Then
1335                         MInspRetryExitFlg = 1
1336                     Else
1337                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
1338                         If MRetryCnt = 0 Then
1339                             MInspRetryExitFlg = 1
1340                         Else
1341                             'Retry�ց@���̑O��Delay
1342                             Dly 0.3
1343                         EndIf
1344                     EndIf
1345                     '
1346                 WEnd
1347             EndIf
1348         EndIf
1349         '
1350         '
1351         '
1352         MNum% = MNum% + 1                                           '����Step+1
1353         '�����I���m�F�@�����I���t���O�Z�b�g
1354         If (MInspCnt% < MNum% ) Then
1355             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
1356         EndIf
1357         'NG���������s������
1358         If MInspErrNum <> 0 Then                                    'NG����?
1359             If MNgContinue% <> 1 Then                               'NG���s?
1360                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
1361             EndIf
1362         EndIf
1363     WEnd
1364     '
1365     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
1366     If 0 < MZAxis% Then
1367         PCurrentPos = P_Curr                                        '���݈ʒu�擾
1368         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
1369         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
1370     EndIf
1371     '
1372     '�߂�l�ݒ�
1373     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
1374         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
1375     Else
1376         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
1377         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
1378         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
1379     EndIf
1380     Fine 0 , P
1381     '
1382 FEnd
1383 '
1384 ' ��ISInspection ���g�p�ɂ��폜 2022/07/16 M.H
1385 '
1386 '��InitialZoneB
1387 ''' <summary>
1388 ''' ����~��̕��A����
1389 ''' 1)���ޔ��@Z������Ɉړ�
1390 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
1391 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
1392 ''' 4)�C�j�V�����|�W�V�����ֈړ�
1393 ''' </summary>
1394 ''' <remarks>
1395 ''' Date : 2022/04/11 : N.Watanabe
1396 ''' </remarks>
1397 Function V fnInitialZoneB()
1398     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
1399 '
1400 '�p�����[�^
1401     Ovrd 5
1402 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
1403 '    Cmp Pos, &B100011
1404 '
1405 '���A����J�n
1406 '
1407 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
1408 *RecoveryChuckOpen
1409     PActive = P_Curr          '���݈ʒu���擾
1410     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
1411 'PMechaOnRoboSet(DVD���J�u����)�́A�`���b�N���
1412     If (PActive.X <= PMechaOnRoboSet.X + 1.0) And (PActive.X >= PMechaOnRoboSet.X -1.0) Then
1413         If (PActive.Y <= PMechaOnRoboSet.Y + 1.0) And (PActive.Y >= PMechaOnRoboSet.Y -1.0) Then
1414             If (PActive.Z <= PMechaOnRoboSet.Z + 1.0) And (PActive.Z >= PMechaOnRoboSet.Z -1.0) Then
1415                 MRecoveryChuckOpen = 1
1416             EndIf
1417         EndIf
1418     EndIf
1419 'PMechaOnRoboGet(DVD���J�󂯎��ʒu)�́A�`���b�N���
1420     If (PActive.X <= PMechaOnRoboGet.X + 1.0) And (PActive.X >= PMechaOnRoboGet.X -1.0) Then
1421         If (PActive.Y <= PMechaOnRoboGet.Y + 1.0) And (PActive.Y >= PMechaOnRoboGet.Y -1.0) Then
1422             If (PActive.Z <= PMechaOnRoboGet.Z + 1.0) And (PActive.Z >= PMechaOnRoboGet.Z -1.0) Then
1423                 MRecoveryChuckOpen = 1
1424             EndIf
1425         EndIf
1426     EndIf
1427     If MRecoveryChuckOpen = 1 Then
1428         M_Out(12256) = 0        'DVD�`���b�N��OFF
1429         M_Out(12257) = 1        'DVD�`���b�N�JON
1430         M_20# = 0               'KEY���͏�����
1431         MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1432         If MRtn = 0 Then
1433             fErrorProcess(11,270,284,0)
1434             If M_20# = MNext% Then M_20# = MClear%
1435             If M_20# = MAbout% Then GoTo *RecoveryEnd
1436             If M_20# = MNgProcess% Then GoTo *RecoveryEnd
1437             If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
1438         Else
1439             M_Out(12257) = 0        'DVD�`���b�N�JOFF
1440         EndIf
1441     EndIf
1442 '
1443 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
1444 '
1445 'PMechaOnRoboSet(Get)�`PMechaOnRoboSet(Get)_1�̃G���A�ɂ���Ƃ��́APMechaOnRoboSet_1��
1446 '�EPMechaOnRoboSet
1447 '�EPMechaOnRoboSet_1
1448 '�EPMechaOnRoboGet
1449 '�EPMechaOnRoboGet_1
1450 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
1451     PActive = P_Curr                    '���݈ʒu���擾
1452     If (PActive.X >= -150) And (PActive.X <= -60) Then
1453         If (PActive.Y >= 540) And (PActive.Y <= 570) Then
1454             If (PActive.Z >= 290) And (PActive.Z <= 320) Then
1455                 Mvs PMechaOnRoboSet_1
1456                 Dly 1.0
1457             EndIf
1458         EndIf
1459     EndIf
1460 '
1461 'PMechaOnRoboSet(Get)_1�`PMechaOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APMechaOnRoboSet_2��
1462 '�EPMechaOnRoboSet_1
1463 '�EPMechaOnRoboSet_2
1464 '�EPMechaOnRoboGet_1
1465 '�EPMechaOnRoboGet_2
1466 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
1467 '    PActive = P_Curr                    '���݈ʒu���擾
1468 '    If (PActive.X >= -90) And (PActive.X <= 50) Then
1469 '        If (PActive.Y >= 300) And (PActive.Y <= 570) Then
1470 '            If (PActive.Z >= 290) And (PActive.Z <= 430) Then
1471 '                Mvs PMechaOnRoboSet_2
1472 '                Dly 1.0
1473 '            EndIf
1474 '        EndIf
1475 '    EndIf
1476 '
1477 '���ޔ�
1478     PActive = P_Curr
1479     Pmove = PActive
1480     Pmove.Z = 500           '���ޔ�����ꗥ�̍���
1481      If PActive.X < -400 Then
1482         Pmove.Z =290        '����(F)�󂯎��ʒu��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
1483     EndIf
1484     If PActive.X > 400 Then
1485         Pmove.Z =400        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
1486     EndIf
1487     If PActive.Z < Pmove.Z Then
1488         Mvs Pmove
1489     EndIf
1490     Dly 1.0
1491 'J1���ȊO��ޔ��|�W�V�����ֈړ�
1492     JActive = J_Curr
1493     Jmove = JTaihi
1494     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
1495     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
1496     Mov Jmove
1497     Dly 1.0
1498 'J1���݂̂�ޔ��|�W�V�����ֈړ�
1499     Mov JTaihi
1500     Dly 1.0
1501 '�C�j�V�����|�W�V�����ֈړ�
1502     Mov PInitialPosition
1503     Cmp Off
1504     Ovrd 100
1505 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n         '2022/04/20 �t�@���N�V�����̊O�ֈړ� �n��
1506 '    If M_In(11856) = 0 Then                 ' ��~���̂�
1507 '        M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
1508 '        MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
1509 '        If MRet = 0 Then
1510 '        Else
1511 '            M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
1512 '        EndIf
1513 '    EndIf
1514     fErrorProcess(11,253,281,0)
1515     Exit Function
1516 *RecoveryEnd
1517 FEnd
1518 '
1519 '
1520 '��fnAutoScreenComment
1521 ''' <summary>
1522 ''' ���C����ʂ̓���󋵕\��
1523 ''' �R�����gD1005�̐ݒ�
1524 ''' </summary>
1525 '''<param name="McommentD1005%">�R�����gID</param>
1526 ''' <remarks>
1527 ''' Date   : 2021/07/07 : M.Hayakawa
1528 ''' </remarks>
1529 Function fnAutoScreenComment(ByVal McommentD1005%)
1530     M_Out16(12576) = McommentD1005%
1531 FEnd
1532 '
1533 '��fnRoboPosChk
1534 ''' <summary>
1535 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
1536 ''' </summary>
1537 '''<param name="MINNumber%">���͔ԍ�</param>
1538 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
1539 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
1540 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
1541 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
1542 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
1543 ''' <remarks>
1544 ''' Date   : 2021/07/07 : M.Hayakawa
1545 ''' </remarks>
1546 Function M% fnRoboPosChk
1547     fnRoboPosChk = 0
1548     MRet = fnStepRead()
1549     '�����ʒu�łȂ��Ɣ��f�����ꍇ
1550     '�E�B���h��ʐ؊���
1551     If MRBTOpeGroupNo > 5 Then
1552         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1553         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
1554         Dly 0.2
1555         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
1556         Dly 1.5
1557         '
1558         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
1559         '
1560         MLoopFlg% = 1
1561         While MLoopFlg% = 1
1562             '
1563             '
1564             MKeyNumber% = fnKEY_WAIT()
1565             Select MKeyNumber%
1566                 Case Is = MAbout%       '��~
1567                     M_20# = MAbout%
1568                     MLoopFlg% = -1
1569                     Break
1570                 Case Is = MNext%        '����
1571                     'MLoopFlg% = -1
1572                     Break
1573                 Case Is = MContinue%    '�p��
1574                     M_20# = MContinue%
1575                     MLoopFlg% = -1
1576                     Break
1577                 Default
1578                     Break
1579             End Select
1580         WEnd
1581     EndIf
1582     '
1583     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
1584         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
1585         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
1586         Select MRBTOpeGroupNo
1587             Case Is = 5                          '�������Ȃ�
1588                 Break
1589             Case Is = 10                         '�����ʒu�֖߂�
1590                 'Mov PTEST001
1591                 Break
1592             Case Is = 15                         '�����ʒu�֖߂�
1593                 'Mov PTEST002
1594                 Dly 0.5
1595                 'Mov PTEST001
1596                 Dly 0.5
1597                 Break
1598             Default
1599                 Break
1600         End Select
1601         '
1602         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
1603         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
1604         MRBTOpeGroupNo = 5
1605         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
1606         Dly 1.0
1607         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
1608         fnRoboPosChk = 1                        '�����ʒu������s
1609         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
1610     EndIf
1611     Exit Function
1612 FEnd
1613 '
1614 '��frInCheck
1615 ''' <summary>
1616 ''' �Z���T�[IN�`�F�b�N
1617 ''' </summary>
1618 '''<param name="MINNumber%">���͔ԍ�</param>
1619 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
1620 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
1621 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
1622 ''' <remarks>
1623 ''' Date   : 2021/07/07 : M.Hayakawa
1624 ''' </remarks>
1625 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
1626     M_Timer(4) = 0
1627     MloopFlg = 0
1628     While MloopFlg = 0
1629         MCrtTime& = M_Timer(4)
1630         If M_In(MINNumber%) = MCMPFLG% Then
1631             MloopFlg = 1
1632             frInCheck = 1
1633         ElseIf MCrtTime& > MTimeCnt& Then
1634             MloopFlg = 1
1635             frInCheck = 0
1636         EndIf
1637     WEnd
1638 FEnd
1639 '-----------------------------------------------
1640 '
1641 '�˂����ߋ@�ʐM�m�F
1642 '
1643 '-----------------------------------------------
1644 Function M% fScewTcomChk
1645     fScewTcomChk = 0
1646     '�ʐM�m�F���M
1647     M_Out(MOUT_ScwT_ComChk%) = MOn%
1648     '�ʐM�m�F��M�ҋ@
1649     Wait M_In(MIN_ScwT_comOK%) = MOn%
1650     '�ʐM�m�F���M�I��
1651     M_Out(MOUT_ScwT_ComChk%) = MOff%
1652  '
1653 FEnd
1654 '
1655 '
1656 '-----------------------------------------------
1657 '
1658 '�˂����ߊJ�n���M
1659 '
1660 '-----------------------------------------------
1661 Function M% fScewTStart
1662     fScewTStart = 0
1663     '�˂����ߊJ�n�ҋ@����M
1664     Wait M_In(MIN_ScwT_STRec%) = MOn%
1665     Dly 0.1
1666     '�˂����ߊJ�n��M�𑗐M
1667     M_Out(MOUT_ScwT_ST%) = MOn%
1668     Dly 0.5
1669     'Wait M_In(MTEST_KEY%) = MOn%
1670     '�˂����ߊJ�n���M�I��
1671     M_Out(MOUT_ScwT_ST%) = MOff%
1672     '
1673 FEnd
1674 '
1675 '
1676 '-----------------------------------------------
1677 '
1678 '�˂����ߊ�����M
1679 '
1680 '-----------------------------------------------
1681 Function M% fScewTFinish
1682     fScewTFinish = 0
1683     '�˂����ߊ����ҋ@����M
1684     Wait M_In(MIN_ScwT_Fin%) = MOn%
1685     Dly 0.1
1686     '�˂����ߊ�����M�𑗐M
1687     M_Out(MOUT_ScwT_FinOK%) = MOn%
1688     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
1689     '�˂����ߊJ�n���M�I��
1690     M_Out(MOUT_ScwT_FinOK%) = MOff%
1691     'Wait M_In(MTEST_KEY%) = MOn%
1692     '
1693 FEnd
1694 '
1695 '
1696 '-----------------------------------------------
1697 '
1698 '����xx��~��M
1699 '
1700 '-----------------------------------------------
1701 Function M% fScewTCaseStop(ByVal MCase%())
1702     fScewTCaseStop = 0
1703     '����xx��~����M
1704     Wait M_In(MCase%(1)) = MOn%
1705     Dly 0.1
1706     '����xx��~��M�𑗐M
1707     M_Out(MCase%(2)) = MOn%
1708     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
1709     '�˂����ߊJ�n���M�I��
1710     M_Out(MCase%(2)) = MOff%
1711     '
1712 FEnd
1713 '
1714 '
1715 '��fScrewTighenRoboCheck
1716 '<summary>
1717 '�˂����{�Ď�
1718 '</summary>
1719 '<param name = "MStopNum%"> ��~�ԍ�</param>
1720 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
1721 '<make>
1722 '2021/12/2 �����V��
1723 '</make>
1724 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
1725     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
1726     fScrewTighenRoboCheck = 1
1727     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
1728     MCheck% = 0
1729     While MScrewTighenRoboFlg% = 1
1730         MCheck% = M_In16(11904)
1731         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
1732             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
1733             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1734         EndIf
1735         If MCheck% <> 0 Then
1736             fScrewTighenRoboError(MCheck%)
1737             Select M_20#
1738                 Case MAbout%            '��~�������ꂽ�ꍇ
1739                     M_Out(12869) = 1 Dly 1.0
1740                     MScrewTighenRoboFlg% = 0
1741                     fScrewTighenRoboCheck = 0   '�ُ�I��
1742                     Break
1743                 Case MNgProcess%        'NG�������ꂽ�ꍇ
1744                     M_Out(12873) = 1 Dly 1.0
1745                     MScrewTighenRoboFlg% = 0
1746                     fScrewTighenRoboCheck = 0   '�ُ�I��
1747                     Break
1748                 Case MContinue%             '���g���C�������ꂽ�ꍇ
1749                     M_20# = MClear%         'M_20#������
1750                     M_Out(12871) = 1 Dly 1.0
1751                     Break
1752                 Case MNext%                 '���ւ������ꂽ�ꍇ
1753                     M_20# = MClear%         'M_20#������
1754                     M_Out(12874) = 1 Dly 1.0
1755                     Break
1756             End Select
1757             Dly 0.5
1758         EndIf
1759     WEnd
1760 FEnd
1761 '��fScrewTighenRoboError
1762 '<summary>
1763 '�˂����{�G���[����
1764 '</summary>
1765 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
1766 '<make>
1767 '2021/12/2 �����V��
1768 '</make>
1769 Function fScrewTighenRoboError(ErrorCode%)
1770     MCommentD1001 = ErrorCode% + 300
1771     fErrorProcess(11,MCommentD1001,0,0)
1772 FEnd
1773 '
1774 '��fErrorProcess
1775 '<summary>
1776 '�G���[����
1777 '</summary>
1778 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
1779 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
1780 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
1781 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
1782 '<make>
1783 '2021/11/5 �����V��
1784 '</make>
1785 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
1786     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
1787     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
1788     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
1789     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
1790     MKeyNum% = 0
1791 *RETRY_ERR_PROCESS
1792      M_20# = MClear%     '������
1793 '        '�G���[�����L�q
1794         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
1795 '        'GOT KEY���͑҂�
1796         MKeyNum% = fnKEY_WAIT()
1797 '        '
1798         If MKeyNum% = MAbout% Then   '��~��I�������ꍇ
1799             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1800             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1801             Break
1802          '
1803         ElseIf MKeyNum% = MContinue% Then   '�p����I�������ꍇ
1804             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1805             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1806         '
1807         ElseIf MKeyNum% = MNext% Then   '���ւ�I�������ꍇ
1808             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1809             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1810          '
1811         ElseIf MKeyNum% = MNgProcess% Then   '��~��I�������ꍇ
1812             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1813             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1814             Break
1815         '
1816         EndIf
1817         '
1818         If M_20# = MClear% Then *RETRY_ERR_PROCESS
1819 FEnd
1820 '
1821 '��fnTorqueCheck
1822 ''' <summary>
1823 ''' �g���N�`�F�b�N����p�̃��C��
1824 ''' </summary>
1825 ''' <remarks>
1826 ''' Date   : 2021/12/21 : H.AJI
1827 ''' </remarks>'
1828 Function M% fnTorqueCheck
1829     '�g���N�`�F�b�N�����M  �����n��~
1830     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
1831     '
1832     fnTorqueCheck = 0
1833     Ovrd 20
1834     Mov PInitialPosition              '�����ʒu�ړ�
1835     Accel 100 , 20
1836     Mvs PHandChange                   '�n���h�����ʒu
1837     Accel 100 , 100
1838     Ovrd 100
1839     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1840     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
1841     Dly 0.2
1842     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
1843     '
1844     'M6340  �g���N�`�F�b�N��M
1845     'Dly 5.0
1846     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
1847     Dly 1.0
1848     M_Out(12340) = 0
1849     '
1850     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
1851     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
1852    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
1853     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
1854     '
1855     '
1856     MLoopFlg = 1
1857     While MLoopFlg = 1
1858         '
1859 '        Mov PInitialPosition              '�����ʒu�ړ�
1860         '
1861         MKeyNumber = fnKEY_WAIT()
1862         Select MKeyNumber
1863             Case Is = 1           '��~
1864                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
1865                 Dly 1.0
1866                 M_Out(12343) = 0
1867                 Ovrd 20
1868                 'Mov PTicketRead_1
1869                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
1870                 Wait M_In(11859) = 1      '�˂����{����̏I��
1871                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
1872                 Ovrd 100
1873                 M_20# = 1
1874                 MLoopFlg = -1
1875                 Break
1876             Case Is = 2           '����
1877                 Break
1878             Case Is = 3           '�p��
1879                 Break
1880             Case Is = 4           '�g���N�`�F�b�N�J�n
1881                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
1882                 Dly 1.0
1883                 M_Out(12342) = 0
1884                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
1885                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
1886                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
1887                 EndIf
1888                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
1889                 'MRet = fnMoveTorquePosi()
1890                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
1891                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1892                 Break
1893             Default
1894                 Break
1895         End Select
1896     WEnd
1897     '
1898     '�g���N�`�F�b�N����~���M
1899     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
1900     '
1901     '���{�b�g�̈ʒu�����ɖ߂�
1902     Mvs PInitialPosition            '�C�j�V�����|�W�V����
1903     '
1904     '
1905  FEnd
1906  '
1907 '
1908 '
1909 '---------------------------
1910 '
1911 '    ���C����ʂ̕\���A��\���ݒ�
1912 '         �R�����gD1001, D1002, D1003�̐ݒ�
1913 '           MWindReSet = 0     ��ʔ�\��
1914 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
1915 '           MWindErrScr = 10    �G���[��� D1001, D1002
1916 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
1917 '
1918 '---------------------------
1919 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1920     fnMainScreenOpen = 0
1921     '
1922    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1923         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
1924     EndIf
1925     '
1926     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1927         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
1928     EndIf
1929     '
1930     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1931         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
1932     EndIf
1933     '
1934     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
1935     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
1936     Dly 0.5
1937     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
1938 FEnd
1939 '
1940 '��Main
1941 ''' <summary>
1942 ''' �g���N�`�F�b�N������
1943 ''' </summary>
1944 ''' <remarks>
1945 ''' Date   : 2021/12/21 : H.AJI
1946 ''' </remarks>'
1947 Function M% fnScrewMTorque
1948     fnScrewMTorque = 0
1949     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
1950     Wait M_In(11857) = 1                     '��M����
1951     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
1952     Dly 2.0
1953 FEnd
1954 '
1955 '��Main
1956 ''' <summary>
1957 ''' �g���N�`�F�b�N������
1958 ''' </summary>
1959 ''' <remarks>
1960 ''' Date   : 2021/12/21 : H.AJI
1961 ''' </remarks>'
1962 Function M% fnMoveTorquePosi
1963      fnMoveTorquePosi = 0
1964      Ovrd 50
1965     'Mov PTorquePosi000 '�g���N�`�F�b�N����ʒu�ֈړ�
1966      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
1967     'Mov PTorquePosi020 '�g���N�`�F�b�N�r�b�g�W���C���g���
1968     '
1969     '
1970      '�ȉ��͈������񂪍쐬�����g���N�`�F�b�N�v���O����
1971     '
1972     Spd M_NSpd
1973 '-------------      �h���C�o�[RST
1974     M_Out(12240)=0     '�h���C�o�[OFF CCW
1975     M_Out(12241)=0     '�h���C�o�[OFF CW
1976     M_Out(12242)=0     '�h���C�o�[���� C1
1977     M_Out(12243)=0     '�h���C�o�[���� C2
1978     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
1979 '---------------------------------------
1980 '---------------------------------------
1981     Fsc Off            '�͊o�Z���T�@Off  STEP1�͕s�v
1982 '--------------------------------------------------------------
1983 '--------------------------------------------------------------
1984 '[P-11]
1985 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
1986     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
1987    'Mov PTorquePosi020, -10                    ' �g���N-1�@�u���ʒu��� 10mm �ֈړ�
1988     Dly 0.1
1989 '-----------------------
1990    'Cnt 0                           'Cnt����-2�@�I��
1991 '-----------------------
1992     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
1993     Dly 0.2
1994 '-----------------------
1995     M_Out(12242)=1                   '�h���C�o�[�Z�b�g C1
1996     Dly 0.1
1997     M_Out(12243)=1                   '�h���C�o�[�Z�b�g C2 (�o���N3)
1998     Dly 0.1
1999     M_Out(12245)=1                   '�v���O����2�Z�b�g F1  M�l�W
2000     Dly 0.1
2001     'M_Out(12241)=1                   '�h���C�o�[ON  CW
2002    M_Out(12241)=0                   '�h���C�o�[OFF  CW
2003     'Dly 0.1
2004 '--------------------------------
2005     Ovrd 40
2006    'Dly 0.1
2007 '--------------------------------  �l�W���ߑ��x�ݒ�
2008     Spd 14                            '���C�h 100-40 100% :Spd 12
2009     Dly 0.1
2010 '--------------------------------
2011 '--------------------------------
2012 '---------------------------------�y�˂����ߓ���z
2013 '
2014     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2015    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2016     Dly 0.3                          '�������҂�
2017    M_Out(12241)=1                   '�h���C�o�[ON  CW
2018 '
2019     Wait M_In(11584)=1                '����/�G���[���o
2020     Dly 0.1
2021     Spd M_NSpd
2022    'Ovrd 20
2023     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2024     Wait M_In(11257)=1                '�l�W����SC
2025 '---------------------------------
2026     Dly 0.1
2027     M_Out(12241)=0                    '�h���C�o�[OFF CW
2028     Dly 0.1
2029     M_Out(12242)=0                    '�h���C�o�[���� C1
2030     Dly 0.1
2031     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2032     Dly 0.1
2033     M_Out(12245)=0                    '�v���O����2���� F1
2034 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2035 '
2036     Mvs PTorqueCheck,-60                       '������mov ����ύX
2037     Dly 0.1
2038 '--------------------------------------------------------------
2039    'Ovrd 80
2040 '--------------------------------------------------------------
2041 '---------------------------------------
2042 '---------------------------------------
2043 '---------------------------------------�G���[���E����
2044    *LBL1
2045    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2046    Mvs ,-100
2047    M_Out(12241)=0     '�h���C�o�[OFF CW
2048    Dly 0.1
2049    M_Out(12242)=0     '�h���C�o�[���� C1
2050    Dly 0.1
2051    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2052    Dly 0.1
2053    M_Out(12245)=0     '�v���O�������� F1
2054 '---------------------------------------
2055 '---------------------------------------
2056 '-------------
2057    'Mov PInitPos19049
2058    Dly 0.1
2059 '
2060 '
2061 '
2062 FEnd
2063 '
2064 ''��Main
2065 ''' <summary>
2066 ''' �g������p�̃��C��
2067 ''' </summary>
2068 ''' <remarks>
2069 ''' Date   : 2021/07/07 : M.Hayakawa
2070 ''' </remarks>'
2071 Function Main
2072     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2073     '
2074     If M_Svo=0 Then
2075         Servo On
2076     EndIf
2077     Wait M_Svo=1
2078 '�g���X�^�[�g���t�����v���p���XON (�ʃX���b�g��8����v���ɕύX�j
2079 '    M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2080 '�p�g���C�g����
2081     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2082     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2083     '
2084     M_20# = 0                                   'KEY���͏�����
2085 '    M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����(�ʒu�ړ�1/18����)
2086     MRet% = 0
2087 ' �����ʒu�̊m�F�ƈړ�(����ێ��̂܂܃p���b�g�̂ݓ���̂��ߍ폜)
2088 '
2089 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2090     PActive = P_Curr                    '���݈ʒu���擾
2091     MRecoveryPass% = 0
2092     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2093         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2094             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2095                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2096             EndIf
2097         EndIf
2098     EndIf
2099     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2100         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2101             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2102                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2103             EndIf
2104         EndIf
2105     EndIf
2106     If (PActive.X <= PMechaOnJigGet_3.X + 1.0) And (PActive.X >= PMechaOnJigGet_3.X -1.0) Then
2107         If (PActive.Y <= PMechaOnJigGet_3.Y + 1.0) And (PActive.Y >= PMechaOnJigGet_3.Y -1.0) Then
2108             If (PActive.Z <= PMechaOnJigGet_3.Z + 1.0) And (PActive.Z >= PMechaOnJigGet_3.Z -1.0) Then
2109                 MRecoveryPass% = 1       'DVD���ʒu���ʒu�͕��A����p�X
2110             EndIf
2111         EndIf
2112     EndIf
2113     If MRecoveryPass% = 0 Then
2114        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2115     EndIf
2116 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n(����ێ��̂܂܃p���b�g�̂ݓ���̂��ߍ폜)
2117 '
2118     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2119         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2120 '�g���N�`�F�b�N
2121         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2122             MRet% = fnTorqueCheck()
2123             Break
2124         Else
2125 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F'12/21�R�����g�A�E�g(����)
2126 '                MRtn = InspInit()               '�摜��������������
2127 '            EndIf
2128             '
2129            M_20# = MClear%                    '������
2130 '�g���J�n
2131             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2132                 fnAssyStart()
2133             Else
2134                 M_20# = MPass%
2135             EndIf
2136             M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����(�ʒu�ړ�1/18����)
2137 '�g���I�����t����
2138             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2139             Wait M_In(11572) = 1            '���t�擾����
2140             Dly 0.1
2141             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2142 '���t�^�[���j�b�g�ւ�OUT
2143             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2144             fnAutoScreenComment(89)         'AUTO��� �g����������
2145             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
2146 'OK/NG�t���O�o��
2147             If M_20# = MAssyOK% Or M_22# = MIrregular% Then
2148                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
2149             ElseIf M_20# = MPass% Then
2150                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
2151             EndIf
2152 'PIAS�ɑg������������  CD�ɂ��폜 2022/07/16 M.H
2153             '
2154             '�g���I�����t��������
2155             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
2156             '�������A�g��OK���A�g��NG��������
2157 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
2158             '
2159 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F'�R�����g�A�E�g12/21(����)
2160 '                '�摜�����I������
2161 '                MRtn = InspQuit()
2162 '            EndIf
2163         EndIf
2164         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
2165     EndIf
2166 '�p�g���C�g����
2167     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
2168     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
2169 'GOT�\��
2170     fnAutoScreenComment(93)  'AUTO��� �H������
2171 FEnd
2172 End
2173 '
2174 '
2175 '���܂��Ȃ��R�����g
2176 '��΍폜�����
2177 '
2178 '
2179 '
2180 '
2181 '
PInspPosition(1)=(+600.00,-152.00,+403.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+6.08,+550.00,+537.59,+145.00,+0.00,-90.00,+0.00,+0.00)(7,0)
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
PTemp=(+303.63,-1.43,+467.78,-0.01,+90.00,+0.00,+0.00,+0.00)(6,0)
PScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
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
PActive=(+303.63,-1.43,+467.78,-0.01,+90.00,+0.00)(6,0)
Pmove=(+244.33,+180.27,+500.00,+23.65,+90.00,+60.35)(6,0)
PBracketFCheck=(-21.45,+489.13,+480.00,-180.00,+0.00,+0.00)(7,0)
PBracketFCheck1=(+2.27,+435.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck2=(+2.27,+535.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck_2=(-171.46,+546.61,+480.00,-180.00,+0.00,-90.00)(7,0)
PBracketFCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet=(-605.12,-91.91,+244.44,+179.99,+0.37,+91.79)(7,1)
PBracketFGet_1=(-605.12,-91.91,+269.93,+179.98,+0.37,+91.79)(7,1)
PBracketFGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketFGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketFGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet=(-197.56,+547.22,+455.08,-179.60,-0.16,-89.30)(7,0)
PBracketFSet_1=(-197.56,+547.22,+470.00,-179.60,-0.16,-89.30)(7,0)
PBracketFSet_2=(-197.56,+547.22,+540.00,-179.60,-0.16,-89.30)(7,0)
PBracketFSet_3=(-26.65,+244.35,+539.98,-179.99,-0.01,-85.26)(7,0)
PBracketFSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck=(-21.45,+489.13,+480.00,+180.00,+0.00,+0.00)(7,0)
PBracketRCheck1=(+6.08,+420.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck2=(+6.08,+550.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck_2=(-177.57,+546.61,+479.97,-180.00,+0.00,-90.00)(7,0)
PBracketRCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet=(-539.54,-90.81,+244.62,-179.85,-0.06,+92.52)(7,1)
PBracketRGet_1=(-539.54,-90.81,+290.00,-179.85,-0.06,+92.52)(7,1)
PBracketRGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketRGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketRGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet=(-152.04,+544.93,+454.54,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_1=(-152.04,+544.93,+460.00,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_2=(-152.04,+544.93,+520.00,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_3=(-26.65,+244.35,+539.98,-179.99,-0.01,-85.26)(7,0)
PBracketRSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PHandChange=(+347.78,-1.40,+382.90,-90.00,+89.23,-89.99)(6,0)
PInitialPosition=(+303.63,-1.43,+467.78,+165.41,+90.00,+165.42)(6,0)
PInitialPosition1=(+302.91,+7.16,+469.96,-180.00,+0.00,+180.00)(7,0)
PMechaOnJigGet1=(+163.68,+364.78,+175.70,-92.91,+88.79,-2.80)(6,0)
PMechaOnJigGet1_1=(+163.68,+364.78,+200.00,-92.91,+88.79,-2.80)(6,0)
PMechaOnJigGet2=(+163.36,+364.43,+174.78,-73.21,+89.04,+16.52)(6,0)
PMechaOnJigGet2_1=(+163.36,+364.43,+200.00,-73.21,+89.04,+16.52)(6,0)
PMechaOnJigGet_2=(+160.73,+362.85,+350.00,-105.22,+88.56,-15.10)(6,0)
PMechaOnJigGet_3=(+160.71,+268.79,+438.34,-105.40,+88.56,-15.27)(6,0)
PMechaOnJigGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet=(+317.54,+114.36,+240.37,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_1=(+317.54,+114.36,+280.00,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_2=(+317.54,+114.36,+450.00,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet=(-131.85,+556.66,+307.59,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboGet_1=(-80.00,+556.66,+307.20,-179.97,-0.29,-179.49)(7,0)
PMechaOnRoboGet_2=(-80.00,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboGet_3=(+0.00,+350.00,+480.00,-180.00,+0.00,-180.00)(7,0)
PMechaOnRoboGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet=(-131.65,+556.36,+307.20,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboSet_1=(-80.00,+556.36,+307.20,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboSet_2=(+40.77,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead=(+600.00,-152.00,+403.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+600.00,-152.00,+450.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+198.38,+259.83,+450.00,+180.00,+0.00,+128.47)(7,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+36.31,-16.13,+140.49,-0.69,-34.36,+0.57)
Jmove=(+36.31,-15.80,+124.16,+0.00,+71.59,+0.57,+0.00,+0.00)
JTaihi=(+0.00,-15.80,+124.16,+0.00,+71.59,+0.00,+0.00,+0.00)
