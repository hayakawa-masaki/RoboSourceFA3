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
406     MRtn = 1                        'MRtn������
407     *RE_TICKET_READ
408     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
409         MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
410         '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
411         '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
412     EndIf
413     If MRtn = 1 Then GoTo *CompRead
414     Mvs PTicketRead_1                       ' ��U���ɑҔ� �ǉ� 22/07/16 M,H
415 '    fErrorProcess(11,111,254,0)
416     If M_20# = MPass% Then GoTo *AssyEnd    ' ���ւ������ꂽ���̃R�����g�����{�W�����v��ύX 2022/07/20 M.H
417     If M_20# = MNext% Then M_20# = MClear%
418     If M_20# = MAbout% Then GoTo *AssyEnd       ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
419     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
420     If M_20# = MContinue% Then GoTo *RE_TICKET_READ
421     GoTo *ASSY_ERROR_END
422     *CompRead
423     '
424     *INITIAL_CHECK
425     '�n���h�̏�Ԃ��C�j�V�����ɖ߂�
426     MRtn =frInCheck(11264,0,MSETTIMEOUT05&) 'PCB���o(����ƃG���[)
427     If MRtn = 1 Then GoTo *CompCheck_1
428     fErrorProcess(11,0,281,0)
429     If M_20# = MNext% Then M_20# = MClear%
430     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
431     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
432     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
433     *CompCheck_1
434     '
435     If M_In(11266) = 1 Then
436         M_Out(12256) = 0        'PCB�`���b�N�JOFF
437         M_Out(12257) = 1        'PCB�`���b�N��ON
438         Break
439     EndIf
440     If M_In(11268) = 1 Then
441         M_Out(12258) = 0        'PCB�V�����_�[�oOFF
442         M_Out(12259) = 1        'PCB�V�����_�[��ON
443         Break
444     EndIf
445     If M_In(11270) = 1 Then
446         M_Out(12260) = 0        'BtoB�V�����_�[�oOFF
447         M_Out(12261) = 1        'BtoB�V�����_�[��ON
448         Break
449     EndIf
450     '
451     MRtn =frInCheck(11265,1,MSETTIMEOUT05&) 'PCB�`���b�N���o
452     If MRtn = 1 Then GoTo *CompCheck_2
453     fErrorProcess(11,240,281,0)
454     If M_20# = MNext% Then M_20# = MClear%
455     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
456     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
457     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
458     *CompCheck_2
459     '
460     MRtn =frInCheck(11267,1,MSETTIMEOUT05&) 'PCB�V�����_�[�ߌ��o
461      If MRtn = 1 Then GoTo *CompCheck_3
462     fErrorProcess(11,239,281,0)
463     If M_20# = MNext% Then M_20# = MClear%
464     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
467     *CompCheck_3
468     '
469     MRtn =frInCheck(11269,1,MSETTIMEOUT05&) 'BtoB�V�����_�[�ߌ��o
470     If MRtn = 1 Then GoTo *CompCheck_4
471     fErrorProcess(11,243,281,0)
472     If M_20# = MNext% Then M_20# = MClear%
473     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
474     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
475     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
476     *CompCheck_4
477     '
478 '
479 '---------------------------------------------------------------
480     '���͐ݒ�(����)07/30����
481     M_Out(12266) = 0
482     M_Out(12267) = 1
483 '---------------------------------------------------------------
484 '
485 '
486     '���i�ʒu����
487     *RE_POSITIONING        '�ʒu���߃��g���C�p
488     M_Out(12262)=1 Dly 0.3      '�ʒu���߃p���X�M��
489     'Wait M_In(11273)=1          '�ʒu���ߏo�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
490     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
491     If MRtn = 1 Then GoTo *CompPosition_1
492     fErrorProcess(11,231,282,0)
493     If M_20# = MNext% Then M_20# = MClear%
494     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
495     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
496     If M_20# = MContinue% Then GoTo *RE_POSITIONING
497     *CompPosition_1
498     '
499     Dly 0.5
500     M_Out(12264)=1 Dly 0.3      '�v�b�V���p���X�M��
501     'Wait M_In(11275)=1          '�v�b�V���o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
502     MRtn = frInCheck(11275,1,MSETTIMEOUT05&)    '�v�b�V���ʒu�o�[���o(8/26����)
503     If MRtn = 1 Then GoTo *CompPosition_2
504     fErrorProcess(11,232,282,0)
505     If M_20# = MNext% Then M_20# = MClear%
506     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
507     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
508     If M_20# = MContinue% Then GoTo *RE_POSITIONING
509     *CompPosition_2
510     '
511 '    '�ySOC���ID�ǂݍ��݁z
512 '    *RE_SOC_CHECK1
513 '    PInspPosition(1) = PSocPcbRead  'SOC���ID�ǎ�ʒu
514 '    MInspGroup%(1) = 2              '����G�ԍ�
515 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
516 '    If MRtn = 1 Then
517 '        M_Out(12571) = 1        '��ԍ��R�s�[1ON
518 '        Dly 0.3
519 '        M_Out(12566) = 1        '��ԍ��R�s�[�v��ON
520 '        Wait M_In(11581) = 1    '��ԍ��R�s�[����
521 '        M_Out(12571) = 0        '��ԍ��R�s�[1OFF
522 '        M_Out(12566) = 0        '��ԍ��R�s�[�v��OFF
523 '        Dly 0.3
524 '        M_Out(12557)= 1 Dly 0.3         ' ��ԍ��ƍ��r�b�gON
525 '    EndIf
526 '    If MRtn = 1 Then GoTo *CompSocCheck1
527 '    fErrorProcess(11,97,25,0)
528 '    If M_20# = MNext% Then M_20# = MClear%
529 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
530 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
531 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
532 '    *CompSocCheck1
533 '    'SOC���ID��ǂ�
534 ''    Mov PSocPcbRead             'ID�ǂ݈ʒu
535 '    '
536     'SOC������
537     *RE_GET_SOC
538     '
539     Mov PSocGet_2               '��s�b�N�A�b�v���_  Y:�ύX 107.160��106.160
540     M_Out(12256)=0              '��`���b�N�JOFF
541     M_Out(12257)=1              '��`���b�N��ON
542     '
543     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    '�`���b�N�Z���T�[ON
544     If MRtn = 1 Then GoTo *CompGetSOC_1
545     fErrorProcess(11,240,284,0)
546     If M_20# = MNext% Then M_20# = MClear%
547     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549     If M_20# = MContinue% Then GoTo *RE_GET_SOC
550     *CompGetSOC_1
551     '
552     M_Out(12259)=0              'PCB�V�����_�[��OFF
553     M_Out(12258)=1              'PCB�V�����_�[�oON
554     Dly 0.2
555     '
556 '    Wait M_In(11268)=1          'PCB�V�����_�[�o�[�Z���T�[ON
557     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)
558     If MRtn = 1 Then GoTo *CompGetSOC_2
559     fErrorProcess(11,238,284,0)
560     If M_20# = MNext% Then M_20# = MClear%
561     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
562     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
563     If M_20# = MContinue% Then GoTo *RE_GET_SOC
564     *CompGetSOC_2
565     '
566     Mov PSocGet_1               '���� Y:�ύX 107.160��106.160
567     Ovrd 40
568     Mvs PSocGet                 '��s�b�N�A�b�v�ʒu  Y:�ύX 107.170��106.170
569     Dly 0.3
570     Ovrd 5                      '2021-12-19�ǉ� AJ
571     '
572     '
573 '    Wait M_In(11264)=1          'PCB���o�Z���T�[ON
574     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
575     If MRtn = 1 Then GoTo *CompGetSOC_3
576     fErrorProcess(11,0,284,0)
577     If M_20# = MNext% Then M_20# = MClear%
578     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
579     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
580     If M_20# = MContinue% Then GoTo *RE_GET_SOC
581     *CompGetSOC_3
582     '
583     M_Out(12257)=0              '��`���b�N��OFF
584     M_Out(12256)=1              '��`���b�N�JON
585     Dly 0.2                     '�c���͊m�ۗp�f�B���C(22/09/30����)
586     '
587 '    Wait M_In(11266)=1          '�`���b�N�J�Z���T�[ON
588     MRtn = frInCheck(11266 , 1 , MSETTIMEOUT05&)
589     Mvs PSocGet_1               '����  Y:�ύX 107.160��106.160
590     If MRtn = 1 Then GoTo *CompGetSOC_4
591     Mov PSocGet_2               ' Y:�ύX 107.160��106.160
592     fErrorProcess(11,241,284,0)
593     If M_20# = MNext% Then M_20# = MClear%
594     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
595     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
596     If M_20# = MContinue% Then GoTo *RE_GET_SOC
597     *CompGetSOC_4
598     '
599     'Wait M_In(11264)=1          'PCB���o�Z���T�[ON
600     '
601     '���L�APCB����ƃG���[�����ǉ� 2021-12-19 AJ
602     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
603     If MRtn = 1 Then GoTo *CompGetSOC_41
604     fErrorProcess(11,0,284,0)
605     If M_20# = MNext% Then M_20# = MClear%
606     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
607     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
608     If M_20# = MContinue% Then GoTo *RE_GET_SOC
609     *CompGetSOC_41
610     '
611     '
612     'Ovrd 100                   '�ʒu�ύX2021-12-19�ǉ�AJ
613     Ovrd 15                     '�ǉ�1/17����
614     Mov PSocGet_2               '��s�b�N�A�b�v���_
615     Ovrd 100                    '2021-12-19�ǉ�AJ
616     '
617     'SOC��𐻕i��ɒu��
618     Mov PSocSet_2               '��u�����_
619     Mov PSocSet_1               '���i���
620     Dly 0.1
621     Ovrd 40
622     Mvs PSocSet                 '��u���ʒu�i�󒆂ŗ����j
623     M_Out(12256)=0              '��`���b�N�JOFF
624     M_Out(12257)=1              '��`���b�N��ON
625 '
626     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
627     Mvs PSocSet_1               '���i���
628     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
629 '
630     'Wait M_In(11265)=1          '�`���b�N�Z���T�[ON
631     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)
632     If MRtn = 1 Then GoTo *CompGetSOC_5
633     fErrorProcess(11,240,284,0)
634     If M_20# = MNext% Then M_20# = MClear%
635     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
636     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
637     If M_20# = MContinue% Then GoTo *RE_GET_SOC
638     *CompGetSOC_5
639     '
640 '    Wait M_In(11268)=1          'PCB�V�����_�[�o�[�Z���T�[ON�E�E�E����OFF���������}�������s���Ă���
641     MRtn = frInCheck(11268 , 1 , MSETTIMEOUT05&)
642     If MRtn = 1 Then GoTo *CompGetSOC_6
643     fErrorProcess(11,0,284,0)
644     If M_20# = MNext% Then M_20# = MClear%
645     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
646     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
647     If M_20# = MContinue% Then GoTo *RE_GET_SOC
648     *CompGetSOC_6
649     '
650     'Wait M_In(11264)=0          'PCB���o�Z���T�[OFF
651     M_Out(12258)=0              'PCB�V�����_�[�oOFF
652     M_Out(12259)=1              'PCB�V�����_�[��ON
653     '
654 '    Wait M_In(11267)=1          'PCB�V�����_�[�ߒ[�Z���T�[ON
655     MRtn = frInCheck(11267 , 1 , MSETTIMEOUT05&)
656     If MRtn = 1 Then GoTo *CompGetSOC_7
657     fErrorProcess(11,239,284,0)
658     If M_20# = MNext% Then M_20# = MClear%
659     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
660     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
661     If M_20# = MContinue% Then GoTo *RE_GET_SOC
662     *CompGetSOC_7
663     Ovrd 100
664     Mov PSocSet_2               '��u�����_
665 '�ySOC���ID�ǂݍ��݁z
666     *RE_SOC_CHECK1
667     PInspPosition(1) = PSocPcbRead  'SOC���ID�ǎ�ʒu
668     MInspGroup%(1) = 2              '����G�ԍ�
669     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
670 '
671     If MRtn = 1 Then GoTo *CompSocCheck1
672     fErrorProcess(11,97,25,0)
673     If M_20# = MNext% Then M_20# = MClear%
674     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
675     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
676     If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
677     *CompSocCheck1
678 '�y���ID�R�s�[�z
679     *RE_PCB_RECORD
680     M_Out(12571) = 1    ' �̈�1 ��ԍ��R�s�[ (D2600-) On
681     Dly 0.1
682     M_Out(12566) = 1    ' toPLC_��ԍ��R�s�[�v�� On
683 '
684     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��R�s�[���� On
685     If MRtn = 1 Then
686         M_Out(12571) = 0  ' �̈�1 ��ԍ��R�s�[ (D2600-) Off
687         Dly 0.1
688         M_Out(12566) = 0  ' toPLC_��ԍ��R�s�[�v�� Off
689 '        GoTo *RE_PCB_COMPAIRE   ' ��ԍ��ƍ��ɃX�L�b�v
690     Else
691         fErrorProcess(11,39,25,0)
692         If M_20# = MNext% Then M_20# = MClear%
693         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
694         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
695         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
696     EndIf
697 '�y���ID�ƍ��i�R�t���j�z
698     MRetryCount% = 0
699     While (MRetryCount% <= MRetryLimit%)
700         *RE_PCB_COMPAIRE
701         M_Out(12557)= 1 ' ��ԍ��ƍ��r�b�gON
702         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��ƍ�OK(M420) On
703         If MRtn = 1 Then
704             M_Out(12557)= 0     ' ��ԍ��ƍ��r�b�gOff
705             ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
706             MRetryCount% = 99
707         Else
708             If MRetryCount% = MRetryLimit% Then
709                 If M_In(11565) = 1 Then
710                     fErrorProcess(11,37,25,0)
711                 Else
712                     fErrorProcess(11,38,25,0)
713                 EndIf
714                 If M_20# = MNext% Then
715                     M_20# = MClear%
716                     ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
717                     MRetryCount% = 99
718                 EndIf
719                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
720                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
721                 If M_20# = MContinue% Then
722                     MRetryCount% = 0
723                 EndIf
724             Else
725                 ' ���g���C�񐔃C���N�������g
726                 MRetryCount% = MRetryCount% + 1
727                 Dly 0.5 ' ���̍H���ƃ^�C�~���O�����炷�ׂ̃f�B���C
728             EndIf
729         EndIf
730     WEnd
731 '
732 '�ySoc��摜�`�F�b�N�z
733 '    *RE_SOC_CHECK2
734 '    PInspPosition(1) = PSocCheck    'Soc��摜�`�F�b�N�ʒu
735 '    MInspGroup%(1) = 3              '����G�ԍ�
736 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
737 '    If MRtn = 1 Then GoTo *CompSocCheck2
738 '    fErrorProcess(11,43,46,0)
739 '    If M_20# = MNext% Then M_20# = MClear%
740 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
741 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
742 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK2
743 '    *CompSocCheck2
744     '��u���ʒu�摜�����i�s�v�H�j
745     '
746     'SOC���BtoB�v���X
747 '
748 '
749 '
750 '
751     'Mov PSocPress_2             'BtoB�v���X���_
752     *RE_BtoBPRESS   'BtoB�V�����_�[���g���C
753     Mov PSocPress_1             '�v���X���
754     M_Out(12261)=0              '�v���X�V�����_�[��OFF
755     M_Out(12260)=1              '�v���X�V�����_�[�oON
756     Dly 0.2
757     '
758     'Wait M_In(11270)=1          '�v���X�V�����_�[�o�[�Z���T�[ON(�C���ɂ��R�����g�A�E�g(8/27����))
759     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    '�v���X�V�����_�[�o�[�Z���T�[ON(8/27����)
760     If MRtn = 1 Then GoTo *CompPress_1
761     fErrorProcess(11,242,284,0)
762     If M_20# = MNext% Then M_20# = MClear%
763     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
764     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
765     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
766     *CompPress_1
767 '
768 '--------------------------------------------
769     '���͌��o(������)
770     '22/07/29�ǉ� ����
771 '--------------------------------------------
772 *RE_Pa_OUT
773     If M_20# = MContinue% Then M_20# = MClear%
774     M_Out(12266) = 0
775     M_Out(12267) = 1
776     MRtn = frInCheck(11278,1,MSETTIMEOUT05&)    'KA�p���͌��o(22/07/29����)
777     If MRtn = 1 Then GoTo *CompPaOut
778     fErrorProcess(11,200,201,0)
779     If M_20# = MNext% Then M_20# = MClear%
780     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
781     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
782     If M_20# = MContinue% Then GoTo *RE_Pa_OUT
783 *CompPaOut
784     '
785     Ovrd 40
786     Mvs PSocPress               '�v���X�G���h�[�܂ňړ�
787     Dly 0.5
788     'Wait M_In(11270)=1          '�v���X�V�����_�[�o�[�Z���T�[ON�c����OFF��������R�l�N�^�J�o�[�L
789     MRtn = frInCheck(11270,0,MSETTIMEOUT05&)    '�v���X�V�����_�[�o�[�Z���T�[ON(8/27����)
790     If MRtn = 1 Then GoTo *CompPress_2
791     Mvs PSocPress_1              '�v���X���
792     fErrorProcess(11,70,71,0)
793     If M_20# = MNext% Then M_20# = MClear%
794     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
795     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
796     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
797     *CompPress_2
798     '
799     'Dly 0.2
800     '
801     *RE_BtoB_REST
802     M_Out(12260)=0              '�v���X�V�����_�[�oOFF
803     M_Out(12261)=1              '�v���X�V�����_�[��ON
804 '    Wait M_In(11269)=1          '�v���X�V�����_�[�ߒ[�Z���T�[ON
805     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    '�v���X�V�����_�[�ߒ[�Z���T�[ON(8/27����)
806     If MRtn = 1 Then GoTo *CompBtoBRest
807     fErrorProcess(11,243,284,0)
808     If M_20# = MNext% Then M_20# = MClear%
809     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
810     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
811     If M_20# = MContinue% Then GoTo *RE_BtoB_REST
812     *CompBtoBRest
813     '
814     Ovrd 100
815     Mov PSocPress_1             '�v���X���
816     M_Out(12266) = 0
817     M_Out(12267) = 0
818     'Mov PSocPress_2            'BtoB�v���X���_
819     '
820 'Soc��l�W����
821     PGetScrewPos(1) = PScrewSupply_1        ' �˂��s�b�N�A�b�v������
822     PGetScrewPos(2) = PScrewSupply_2        ' �˂������@���_����
823     PGetScrewPos(9) = PScrewSupply_9        ' �l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
824     PGetScrewPos(10) = PScrewSupply         ' �˂��s�b�N�A�b�v������
825     '
826     'Soc��p�l�W�����@�փl�W�����ɍs��
827     *RE_SCREW_GET_1
828     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
829     If MRtn = 1 Then GoTo *CompScrewGet_1
830     If M_20# = MNext% Then M_20# = MClear%
831     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
832     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
833     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
834     *CompScrewGet_1
835     '
836     PScrewPos(1) = PScrewSoc1_1             ' �˂��s�b�N�A�b�v������
837     PScrewPos(2) = PScrewSoc1_0             ' �l�W1���ߊJ�n�ʒu����(10/8 M.H)
838     PScrewPos(10) = PScrewSoc1              ' �˂������@���_����
839     '�@�ԃl�W����
840     M_Out16(12672) = 1              '�l�W���߈ʒu�ԍ����M
841     MRtn = ScrewTight(PScrewPos,1,10.0)
842     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
843     If MRtn = 1 Then GoTo *CompScrew1
844     Mov PInitialPosition
845     fErrorProcess(11,53,52,0)
846     If M_20# = MNext% Then M_20# = MClear%
847     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
848     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
849     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
850     *CompScrew1
851     '
852     'Soc��p�l�W�����@�փl�W�����ɍs��
853     *RE_SCREW_GET_2
854     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
855     If MRtn = 1 Then GoTo *CompScrewGet_2
856     If M_20# = MNext% Then M_20# = MClear%
857     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
858     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
859     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
860     *CompScrewGet_2
861     '
862     PScrewPos(1) = PScrewSoc2_1             ' �˂��s�b�N�A�b�v������
863     PScrewPos(2) = PScrewSoc2_0             ' �l�W2���ߊJ�n�ʒu����(10/8 M.H)
864     PScrewPos(10) = PScrewSoc2              ' �˂������@���_����
865     '�A�ԃl�W����
866     M_Out16(12672) = 2              '�l�W���߈ʒu�ԍ����M
867     MRtn = ScrewTight(PScrewPos,1,10.0)
868     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
869     If MRtn = 1 Then GoTo *CompScrew2
870     Mov PInitialPosition
871     fErrorProcess(11,54,52,0)
872     If M_20# = MNext% Then M_20# = MClear%
873     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
874     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
875     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
876     *CompScrew2
877     '
878     'Soc��p�l�W�����@�փl�W�����ɍs��
879     *RE_SCREW_GET_3
880     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
881     If MRtn = 1 Then GoTo *CompScrewGet_3
882     If M_20# = MNext% Then M_20# = MClear%
883     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
884     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
885     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
886     *CompScrewGet_3
887     '
888     PScrewPos(1) = PScrewSoc3_1             ' �˂��s�b�N�A�b�v������
889     PScrewPos(2) = PScrewSoc3_0             ' �l�W3���ߊJ�n�ʒu����(10/8 M.H)
890     PScrewPos(10) = PScrewSoc3              ' �˂������@���_����
891     '�B�ԃl�W����
892     M_Out16(12672) = 3              '�l�W���߈ʒu�ԍ����M
893     MRtn = ScrewTight(PScrewPos,1,10.0)
894     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
895     If MRtn = 1 Then GoTo *CompScrew3
896     Mov PInitialPosition
897     fErrorProcess(11,55,52,0)
898     If M_20# = MNext% Then M_20# = MClear%
899     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
900     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
901     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
902     *CompScrew3
903     '
904     'Soc��p�l�W�����@�փl�W�����ɍs��
905     *RE_SCREW_GET_4
906     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
907     If MRtn = 1 Then GoTo *CompScrewGet_4
908     If M_20# = MNext% Then M_20# = MClear%
909     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
910     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
911     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
912     *CompScrewGet_4
913     '
914     PScrewPos(1) = PScrewSoc4_1             ' �˂��s�b�N�A�b�v������
915     PScrewPos(2) = PScrewSoc4_0             ' �l�W4���ߊJ�n�ʒu����(10/8 M.H)
916     PScrewPos(10) = PScrewSoc4              ' �˂������@���_����
917     '�C�ԃl�W����
918     M_Out16(12672) = 4              '�l�W���߈ʒu�ԍ����M
919     MRtn = ScrewTight(PScrewPos,1,10.0)
920     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
921     If MRtn = 1 Then GoTo *CompScrew4
922     Mov PInitialPosition
923     fErrorProcess(11,56,52,0)
924     If M_20# = MNext% Then M_20# = MClear%
925     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
926     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
927     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
928     *CompScrew4
929     '
930     'Soc��p�l�W�����@�փl�W�����ɍs��
931     *RE_SCREW_GET_5
932     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
933     If MRtn = 1 Then GoTo *CompScrewGet_5
934     If M_20# = MNext% Then M_20# = MClear%
935     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
936     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
937     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
938     *CompScrewGet_5
939     '
940     PScrewPos(1) = PScrewSoc5_1             ' �˂��s�b�N�A�b�v������
941     PScrewPos(2) = PScrewSoc5_0             ' �l�W5���ߊJ�n�ʒu����(10/8 M.H)
942     PScrewPos(10) = PScrewSoc5              ' �˂������@���_����
943     '�D�ԃl�W����
944     M_Out16(12672) = 5              '�l�W���߈ʒu�ԍ����M
945     MRtn = ScrewTight(PScrewPos,1,10.0)
946     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
947     If MRtn = 1 Then GoTo *CompScrew5
948     Mov PInitialPosition
949     fErrorProcess(11,57,52,0)
950     If M_20# = MNext% Then M_20# = MClear%
951     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
952     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
953     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
954     *CompScrew5
955     '
956     'Soc��p�l�W�����@�փl�W�����ɍs��
957     *RE_SCREW_GET_6
958     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
959     If MRtn = 1 Then GoTo *CompScrewGet_6
960     If M_20# = MNext% Then M_20# = MClear%
961     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
962     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
963     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
964     *CompScrewGet_6
965     '
966     PScrewPos(1) = PScrewSoc6_1             ' �˂��s�b�N�A�b�v������
967     PScrewPos(2) = PScrewSoc6_0             ' �l�W6���ߊJ�n�ʒu����(10/8 M.H)
968     PScrewPos(10) = PScrewSoc6              ' �˂������@���_����
969     '�E�ԃl�W����
970     M_Out16(12672) = 6              '�l�W���߈ʒu�ԍ����M
971     MRtn = ScrewTight(PScrewPos,1,10.0)
972     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
973     If MRtn = 1 Then GoTo *CompScrew6
974     Mov PInitialPosition
975     fErrorProcess(11,58,52,0)
976     If M_20# = MNext% Then M_20# = MClear%
977     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
978     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
979     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
980     *CompScrew6
981 '
982     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
983     Mov PTicketRead_1   '�`�P�b�g�ǂݎ��ʒu���
984     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
985     InitialState()  ' ������Ԃɂ���*AssyEnd
986     M_20# = MAssyOK%              ' ����I������
987     GoTo *fnAssyStart_FEndPosi
988 '
989 *ASSY_ERROR_END
990     fnInitialZone()   ' �����ʒu�Ɉړ�
991 *AssyEnd
992     InitialState()  ' ������Ԃɂ���
993 *fnAssyStart_FEndPosi
994     Exit Function
995 FEnd
996 '
997 '��fnPiasCheck
998 ''' <summary>
999 ''' PIAS�`�P�b�g�Ǎ���
1000 ''' </summary>
1001 ''' <returns>   0 : NG
1002 '''             1 : OK(�Ǎ��݊���)
1003 ''' </returns>
1004 ''' <remarks>
1005 ''' Date   : 2021/07/07 : M.Hayakawa
1006 ''' </remarks>'
1007 Function M% fnPiasCheck
1008     fnPiasCheck = 0
1009     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1010     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1011 '
1012 *RETRY_PIAS
1013     M_20# = MClear%
1014     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1015     '
1016     '�yID�`�P�b�g�ǂݍ��݁z
1017     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1018     MInspGroup%(1) = 1              '����G�ԍ�
1019     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1020 '
1021     '�G���[�̏ꍇ
1022     If MRtn <> 1 Then
1023         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1024         If MRtn <> 1 Then
1025             'D720 -> D1300 �R�s�[�v��
1026             M_Out(12565) = 1
1027             Dly 0.5
1028             M_Out(12565) = 0
1029             '�G���[�����L�q
1030             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1031             'GOT KEY���͑҂�
1032             MKeyNumber = fnKEY_WAIT()
1033             '
1034             Select MKeyNumber
1035                 Case MNext%         '���ւ�I�������ꍇ
1036                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1037                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1038                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1039                     Break
1040                 Case MAbout%        '��~��I�������ꍇ
1041                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1042                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1043                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1044                     Break
1045                 Case MNgProcess%    'NG��I�������ꍇ
1046                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1047                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1048                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1049                     Break
1050                 Case MContinue%     '�p����I�������ꍇ
1051                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1052                     M_20# = MContinue%
1053                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1054                     Break
1055             End Select
1056         EndIf
1057     EndIf
1058 '----------D720 -> D1300 �R�s�[�v��----------
1059     M_Out(12565) = 1
1060     Dly 0.5
1061     M_Out(12565) = 0
1062 '----------�ʐM�m�F������----------
1063     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1064     MRtn = 0                ' ������
1065     M_20# = MClear%         ' ������
1066     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1067     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1068     If MRtn <> 1 Then
1069         If M_20# = MContinue% Then
1070             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1071         Else
1072             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1073         EndIf
1074     EndIf
1075 '----------�H�������m�F----------
1076     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1077     MRtn = 0                ' ������
1078     M_20# = MClear%         ' ������
1079     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1080     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1081     If MRtn <> 1 Then
1082         If M_20# = MContinue% Then
1083             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1084         Else
1085             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1086         EndIf
1087     EndIf
1088     '
1089     fnPiasCheck = 1
1090     *fnPiasCheck_End
1091     Exit Function
1092 FEnd
1093 '
1094 '��fnPCComuCheck
1095 ''' <summary>
1096 ''' PC-PLC�ʐM�`�F�b�N
1097 ''' </summary>
1098 ''' <returns>   0 : NG
1099 '''             1 : OK(�Ǎ��݊���)
1100 ''' </returns>
1101 ''' <remarks>
1102 ''' Date   : 2021/07/07 : M.Hayakawa
1103 ''' </remarks>'
1104 Function M% fnPCComuCheck
1105     fnPCComuCheck = 0
1106     MJudge% = 0                                  '������
1107     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1108     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1109     '
1110     For MStaNo = 0 To 5
1111         '
1112         If M_In(MIN_PIAS_ComOK%) = 1 Then
1113             'PC�ʐMOK(M400)
1114             MJudge% = MOK%
1115             MStaNo = 5
1116             Break
1117         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1118             'toRBT_�ʐM�m�Ftime out
1119             MJudge% = MNG%
1120             MCommentD1001 = 15
1121             MCommentD1002 = 21
1122             MStaNo = 5
1123             Break
1124         Else
1125             'toRBT_�ʐM�m�Ftime out
1126             MJudge% = MNG%
1127             MCommentD1001 = 14
1128             MCommentD1002 = 21
1129             Break
1130         EndIf
1131     Next MStaNo
1132     '
1133     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1134     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1135     '
1136     '�G���[���
1137     If MJudge% <> MOK% Then
1138         M_20# = MClear%     '������
1139         '�G���[�����L�q
1140         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1141         'GOT KEY���͑҂�
1142         MKeyNumber = fnKEY_WAIT()
1143         '
1144         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1145             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1146             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1147             Break
1148         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1149             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1150             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1151             Break
1152         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1153             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1154             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1155             Break
1156         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1157             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1158             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1159             Break
1160         EndIf
1161     Else
1162         'OK�̏ꍇ
1163         fnPCComuCheck = 1
1164     EndIf
1165     Exit Function
1166 FEnd
1167 '
1168 '��fnProcessCheck
1169 ''' <summary>
1170 ''' �H�������m�F
1171 ''' </summary>
1172 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1173 '''             -1�F�O�H������NG  -2�F���H����������
1174 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1175 '''             -5�F���������G���[
1176 ''' </returns>
1177 ''' <remarks>
1178 ''' Date   : 2021/07/07 : M.Hayakawa
1179 ''' </remarks>'
1180 Function M% fnProcessCheck
1181     fnProcessCheck = 0
1182     MJudge% = MNG%      '��UNG���������Ƃ���
1183 '----------�H�������m�F----------
1184     MCommentD1001 = 0   '�R�����g������
1185     For MStaNo = 0 To 5
1186         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1187         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1188         '
1189         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1190             MJudge% = MOK%
1191             fnAutoScreenComment(85)     ' AUTO���
1192             MStaNo = 5
1193             Break
1194         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1195             MFlgLoop% = 0
1196             MJudge% = MNG%
1197             MCommentD1001 = 27
1198             MCommentD1002 = 22
1199             fnAutoScreenComment(94)     ' AUTO���
1200             fnProcessCheck = -2         ' NG��-2��Ԃ�
1201             MStaNo = 5
1202             Break
1203         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1204            MJudge% = MNG%
1205             MCommentD1001 = 31
1206             MCommentD1002 = 22
1207             fnAutoScreenComment(83)     ' AUTO���
1208             fnProcessCheck = -3         ' NG��-3��Ԃ�
1209             MStaNo = 5
1210             Break
1211         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1212             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1213             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1214             MJudge% = MNG%
1215             MCommentD1001 = 32
1216             MCommentD1002 = 22
1217             fnAutoScreenComment(84)     ' AUTO���
1218             fnProcessCheck = -1         ' NG��-1��Ԃ�
1219             Dly 1.0
1220             '�H�������m�FOFF
1221             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1222             Dly 1.0
1223            'MStaNo = 5
1224             Break
1225         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1226             MFlgLoop% = 0
1227             MJudge% = MNG%
1228             MCommentD1001 = 29
1229             MCommentD1002 = 22
1230             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1231             fnProcessCheck = -5         ' NG��-5��Ԃ�
1232             MStaNo = 5
1233             Break
1234         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1235             MJudge% = MNG%
1236             If MCommentD1001 = 32 Then
1237                 '�������Ȃ�
1238             Else
1239                 MCommentD1001 = 26
1240             EndIf
1241             MCommentD1002 = 22
1242             fnProcessCheck = -4         ' NG��-4��Ԃ�
1243             MStaNo = 5
1244             Break
1245         Else
1246             MJudge% = MNG%
1247             MCommentD1001 = 28
1248             MCommentD1002 = 22
1249         EndIf
1250     Next MStaNo
1251     '�H�������m�FOFF
1252     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1253     '�ʉߗ���NG �H�������̏ꍇ
1254     If MJudge% = MPass% Then
1255         M_20# = MPass%
1256     EndIf
1257     '
1258     '�G���[���
1259     If MJudge% <> MOK% Then
1260         M_20# = MClear%     '������
1261         '�G���[�����L�q
1262         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1263         'GOT KEY���͑҂�
1264         MKeyNumber = fnKEY_WAIT()
1265         '
1266         Select MKeyNumber
1267             Case MAbout%        '��~��I�������ꍇ
1268                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1269                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1270                 Break
1271             Case MNext%         '���ւ�I�������ꍇ
1272                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1273                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1274                 Break
1275             Case MContinue%     '�p����I�������ꍇ
1276                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1277                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1278                 Break
1279             Case MNgProcess%    'NG��I�������ꍇ
1280                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1281                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1282                 Break
1283         End Select
1284     Else
1285         fnProcessCheck = 1  ' OK��1��Ԃ�
1286     EndIf
1287     Exit Function
1288 FEnd
1289 '
1290 '��fnPiasWrite
1291 ''' <summary>
1292 ''' Pias �g�����ʏ����ݗv��
1293 ''' </summary>
1294 '''<param name="MFlg%">
1295 '''                 MOK%(1) = �H��������OK��������
1296 '''                 MNG%(0) = �H��������NG��������
1297 '''</param>
1298 '''<returns></returns>
1299 ''' <remarks>
1300 ''' Date   : 2021/07/07 : M.Hayakawa
1301 ''' </remarks>'
1302 Function M% fnPiasWrite(ByVal MFlg%)
1303       fnPiasWrite = 0
1304 *RETRY_PIASWRITE
1305     '
1306     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1307    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1308     If MFlg% = MOK% Then
1309         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1310     Else
1311         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1312     EndIf
1313     Dly 0.1                  '�O�̂���
1314     '
1315     'Pias�֏����݊J�n M305 -> ON
1316     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1317     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1318     '
1319     MJudge% = MNG%
1320     '
1321     For MStaNo = 0 To 5
1322         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1323             MJudge% = MOK%
1324             'MRet = fnAutoScreenComment(85)  'AUTO���
1325             MStaNo = 5
1326             Break
1327         '
1328         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1329             MJudge% = MNG%
1330             'MRet = fnAutoScreenComment(85)  'AUTO���
1331            MCommentD1001 = 34
1332            MCommentD1002 = 25
1333             MStaNo = 5
1334             Break
1335         '
1336         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1337             MJudge% = MNG%
1338             'MRet = fnAutoScreenComment(85)  'AUTO���
1339            MCommentD1001 = 35
1340            MCommentD1002 = 25
1341             MStaNo = 5
1342             Break
1343         '
1344         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1345             MJudge% = MNG%
1346             'MRet = fnAutoScreenComment(85)  'AUTO���
1347            MCommentD1001 = 36
1348            MCommentD1002 = 25
1349             MStaNo = 5
1350             Break
1351         '
1352         Else
1353             MJudge% = MNG%
1354            MCommentD1001 = 42
1355            MCommentD1002 = 25
1356         '
1357         EndIf
1358         '
1359     Next MStaNo
1360     '
1361     'Pias�֏����݊J�n M305 -> OfF
1362     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1363     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1364     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1365     '
1366     '
1367     '�ʉߗ���NG �H�������̏ꍇ
1368     If MJudge% = MPass% Then
1369         M_20# = MPass%
1370     EndIf
1371     '
1372    M_20# = MClear%     '������
1373     '
1374     '�G���[���
1375     If MJudge% < MOK% Then
1376     '
1377 '�c���Ă���������ł͎g�p���Ȃ����x��
1378 *RETRY_ERR_WRITE
1379         M_20# = MClear%     '������
1380         '�G���[�����L�q
1381         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1382         'GOT KEY���͑҂�
1383         MKeyNumber = fnKEY_WAIT()
1384         '
1385         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1386             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1387            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1388             Break
1389         '
1390         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1391             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1392             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1393         '
1394         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1395             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1396             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1397         '
1398         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1399             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1400            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1401             Break
1402         '
1403         EndIf
1404         '
1405         If M_20# = MClear% Then *RETRY_ERR_WRITE
1406         '
1407     EndIf
1408     '
1409     If M_20# = MContinue% Then *RETRY_PIASWRITE
1410     '
1411     fnPiasWrite = 1
1412     Exit Function
1413 FEnd
1414 '
1415 '��fnPCBNumberCheck
1416 ''' <summary>
1417 ''' Pias ��ԍ��ƍ��v��
1418 ''' </summary>
1419 '''<param name="%"></param>
1420 '''<param name="%"></param>
1421 '''<returns></returns>
1422 ''' <remarks>
1423 ''' Date   : 2021/07/07 : M.Hayakawa
1424 ''' </remarks>'
1425 Function M% fnPCBNumberCheck
1426       fnPCBNumberCheck = 0
1427     '
1428 *RETRY_PCBCHECK
1429     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1430     'Pias�֊�ƍ��J�n M310 -> ON
1431     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1432     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1433     '
1434     MJudge% = MNG%
1435     '
1436     For MStaNo = 0 To 5
1437         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1438             MJudge% = MOK%
1439             fnAutoScreenComment(96)  'AUTO���
1440             MStaNo = 5
1441             Break
1442         '
1443         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1444             MJudge% = MNG%
1445             fnAutoScreenComment(97)  'AUTO���
1446             MCommentD1001 = 37
1447             MCommentD1002 = 25
1448             MStaNo = 5
1449             Break
1450         '
1451         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1452             MJudge% = MNG%
1453             fnAutoScreenComment(98)  'AUTO���
1454             MCommentD1001 = 38
1455             MCommentD1002 = 25
1456             MStaNo = 5
1457             Break
1458         '
1459         ElseIf M_In(11580) = 1 Then                         'time out
1460             MJudge% = MNG%
1461             fnAutoScreenComment(99)  'AUTO���
1462             MCommentD1001 = 39
1463             MCommentD1002 = 25
1464             MStaNo = 5
1465             Break
1466         '
1467         Else
1468             MJudge% = MNG%
1469            MCommentD1001 = 41
1470            MCommentD1002 = 25
1471         '
1472         EndIf
1473         '
1474     Next MStaNo
1475     '
1476     'Pias�֊�ƍ��J�n M310 -> OfF
1477     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1478     '
1479     '
1480     '�ʉߗ���NG �H�������̏ꍇ
1481     If MJudge% = MPass% Then
1482         M_20# = MPass%
1483     EndIf
1484     '
1485    M_20# = MClear%     '������
1486     '
1487     '�G���[���
1488     If MJudge% < MOK% Then
1489     '
1490 '�c���Ă���������ł͎g�p���Ȃ����x��
1491 *RETRY_ERR_PCBNUMBER
1492         M_20# = MClear%     '������
1493         '�G���[�����L�q
1494         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1495         'GOT KEY���͑҂�
1496         MKeyNumber = fnKEY_WAIT()
1497         '
1498         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1499             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1500             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1501             Break
1502         '
1503         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1504             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1505             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1506         '
1507         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1508             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1509             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1510         '
1511         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1512             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1513             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1514             Break
1515         '
1516         EndIf
1517         '
1518         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1519         '
1520     EndIf
1521     '
1522     If M_20# = MContinue% Then *RETRY_PCBCHECK
1523     Exit Function
1524 FEnd
1525 '
1526 '��ScrewTight
1527 ''' <summary>
1528 ''' �˂����߂��s��(S�^�C�g)
1529 ''' </summary>
1530 '''<param name="PScrewPos()">
1531 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1532 '''             PScrewPos(2)    �F�˂����߉��_
1533 '''             PScrewPos(10)   �F�˂����ߏI������
1534 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
1535 '''             1:6mm S�^�C�g��l�W
1536 '''             2:8mm P�^�C�g
1537 '''             3:6mm S�^�C�g���l�W
1538 '''             4:13mm S�^�C�g
1539 '''             5:6mm M�l�W
1540 '''</param>
1541 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
1542 '''<returns>����
1543 '''         0=�ُ�I���A1=����I��
1544 '''</returns>
1545 ''' <remarks>
1546 ''' Date   : 2021/07/07 : M.Hayakawa
1547 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
1548 ''' </remarks>'
1549 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
1550     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1551     ScrewTight = 0
1552     MOKNGFlg = 0
1553     Ovrd 100
1554     Mov PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
1555     Fine 0.05 , P
1556     Ovrd MOvrdA%
1557     ' �����ݒ�
1558     Accel 100, 10
1559     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
1560     Mvs PScrewPosition(2)
1561     ' �����������ɖ߂�
1562     Accel
1563     ' ����Ovrd�ݒ�
1564 '    Ovrd MOvrdA%
1565     Ovrd 100
1566     ' Spd�ݒ�
1567 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1568     Spd MFeedSpd
1569     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
1570     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1571     Select MScrewType%
1572         Case 1
1573             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1574             ProgramBankSet(1,1)
1575             Break
1576         Case 2
1577             ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1578             ProgramBankSet(3,1)
1579             Break
1580         Case 3
1581             ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1582             ProgramBankSet(1,1)
1583             Break
1584         Case 4
1585             ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1586             ProgramBankSet(1,1)
1587             Break
1588         Case 5
1589             ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1590             ProgramBankSet(1,1)
1591             Break
1592         Case 6
1593             ' S�^�C�g�F�v���O����1�A�o���N4�ɐݒ�
1594             ProgramBankSet(1,4)
1595             Break
1596         Default
1597             ' �v���O����1�A�o���N�Ȃ��ݒ�
1598             ProgramBankSet(0,0)
1599             Break
1600     End Select
1601 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1602      '�h���C�o�[ON�@CW
1603     M_Out(12241)=1
1604     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1605     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
1606     Dly 0.1
1607     Fine 0 , P
1608     Spd M_NSpd
1609     '
1610     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
1611         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1612         Dly 0.1
1613        ' �v���O�����E�o���N����
1614         ProgramBankSet(0,0)
1615         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1616         Mvs PScrewPosition(10),-80
1617         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1618         M_Out(12249)=1 Dly 0.3
1619         MOKNGFlg = -1
1620         ScrewTight = 0
1621     Else
1622          '�h���C�o�[OFF�@CW
1623         M_Out(12241)=0
1624 '        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
1625 '        Select MScrewType%
1626 '            Case 1
1627 '                ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1628 '                ProgramBankSet(1,3)
1629 '                Break
1630 '            Case 2
1631 '                ' P�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1632 '                ProgramBankSet(3,3)
1633 '                Break
1634 '            Case 3
1635 '                ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
1636 '                ProgramBankSet(1,3)
1637 '                Break
1638 '            Case 4
1639 '                ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
1640 '                ProgramBankSet(1,3)
1641 '                Break
1642 '            Case 5
1643 '                ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
1644 '                ProgramBankSet(1,3)
1645 '                Break
1646 '            Default
1647 '                ' �v���O����1�A�o���N�Ȃ��ݒ�
1648 '                ProgramBankSet(0,0)
1649 '                Break
1650 '        End Select
1651 '         '�h���C�o�[ON�@CW
1652 '        Mvs PScrewPosition(10)
1653 '        M_Out(12241)=1
1654 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1655 '        Fine 0 , P
1656 '
1657          '�h���C�o�[OFF�@CW
1658         M_Out(12241)=0
1659        ' �v���O�����E�o���N����
1660         ProgramBankSet(0,0)
1661         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1662         M_Out(12249)=1 Dly 0.3
1663     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1664         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1665        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1666         'Mvs PScrewPosition(10),-80
1667         ScrewTight = 1
1668     EndIf
1669 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1670 '    Ovrd 10
1671 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1672     Ovrd 100
1673     Exit Function
1674 FEnd
1675 '
1676 '��ScrewGet
1677 ''' <summary>
1678 ''' �˂������@����˂��𓾂�
1679 ''' </summary>
1680 '''<param name="%">
1681 '''         PScrewPos(1)    �F�˂�������̂˂����
1682 '''         PScrewPos(2)    �F�˂���������_
1683 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1684 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1685 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1686 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1687 '''</param>
1688 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1689 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1690 '''<returns>����
1691 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1692 '''</returns>
1693 ''' <remarks>
1694 ''' Date   : 2021/07/07 : M.Hayakawa
1695 ''' </remarks>
1696 '''<update>
1697 '''Date    : 2021/11/15 : ����
1698 '''</update>
1699 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1700     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
1701     ScrewGet = 0
1702     MScrewJudge% = 0
1703     '�˂������평������G���[�`�F�b�N
1704 ' ���b��폜
1705     'Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
1706     For MCnt% = 0 To MFinCnt%
1707         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1708         If MRtn = 0 Then
1709             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1710             ScrewGet = -1
1711             MScrewJudge% = 2
1712         EndIf
1713         Ovrd 100
1714         If FeederScrewSensor% <> 0 Then
1715             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1716                 'Ovrd 30
1717                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1718                 'NG�Ƃ��Ă����̊֐����甲����
1719                 ScrewGet = -2
1720                 MScrewJudge% = 3
1721             EndIf
1722         EndIf
1723         Ovrd 100
1724         Spd M_NSpd
1725         If MScrewJudge% = 0 Then
1726     '        ScrewGet = 0
1727             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1728             Dly 0.3
1729             MScrewCnt% = 0
1730             MFinCnt% = 2
1731             fnAutoScreenComment(521)     '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1732             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1733             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1734             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1735             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1736             'Mvs PScrewPosition(10), 1.2
1737            Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
1738             '�r�b�g��](�����ʒu�ύX)
1739             M_Out(Y60_Driver)=1
1740             M_Timer(4) = 0
1741             MloopFlg = 0
1742             MCntTime& = 0
1743             While MloopFlg = 0
1744                 MCrtTime& = M_Timer(4)
1745                 If MCrtTime& >= 180 Then
1746                     MloopFlg = 1
1747                 EndIf
1748             WEnd
1749             M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1750             '�z���m�F
1751             MRtn = 0
1752             MRtn = frInCheck(11271, 1, MSETTIMEOUT01&)
1753 '            '�r�b�g��](�����ʒu�ύX)
1754 '            M_Out(Y60_Driver)=1
1755 '            Dly 0.2
1756             '
1757             JOvrd M_NJovrd
1758             Spd M_NSpd
1759             '�l�W�z���m�F�ʒu�ړ�
1760             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1761             Mvs PScrewPosition(10), -30  ' �l�W�z���m�F�ʒu
1762            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1763             '�r�b�g��]��~
1764             M_Out(Y60_Driver)=0
1765             '
1766 '            If MRtn = 1 Then           '�ŏ���臒l���m�F���Ȃ�
1767                 '1�b�ԃl�W�z���m�F
1768                 MRtn = frInCheck(11272, 1, MSETTIMEOUT01&)
1769 '            EndIf
1770             'MRtn = 0'�����G���[
1771             '�z���G���[�̏ꍇ
1772             '�l�W���˂����Y�ɖ߂�
1773             If MRtn = 0 Then
1774                 Ovrd 30      '2����5�ɕύX
1775                 '�r�b�g��]��~
1776                 M_Out(Y60_Driver)=0
1777                 '�l�W�����@���
1778                 Mvs PScrewPosition(1)
1779                 '�X�ɏ��
1780                 Mov PScrewPosition(1), -140
1781                 '�l�W�̂Ĉʒu
1782                 MRtn = FnCtlValue2(4)          '�z���G���[���{�P  2022/04/28 �n��
1783                 Mov PScrewPosition(9)
1784                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1785                 '�z��OFF
1786                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1787                 Dly 0.2
1788                 '�j��ON
1789                 M_Out(Y6B_VB1)=1 '�^��j��ON
1790                 '�r�b�g��]
1791                 M_Out(Y61_Driver)=1
1792                 Dly 0.5
1793                 '                '
1794                 Ovrd 100
1795                 JOvrd M_NJovrd
1796                 Spd M_NSpd
1797                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1798                 Mov PScrewPosition(9), 10
1799                 Mov PScrewPosition(9)
1800                 Dly 0.1
1801                 Mov PScrewPosition(9), 10
1802                 Mov PScrewPosition(9)
1803                 '
1804                 '�l�W�����҂�
1805                 Wait M_In(11272) = 0
1806                 '�r�b�g��]��~
1807                 M_Out(Y61_Driver)=0
1808                 Dly 0.1
1809                 '�j��OFF
1810                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1811                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1812                 Mov PScrewPosition(1), -140
1813                 Ovrd 100
1814                 Spd M_NSpd
1815                 '�l�W�����@���
1816                 Mvs PScrewPosition(1)
1817 '                '
1818                 ScrewGet = -3
1819                 If MCnt% = MFinCnt% Then
1820                     MScrewJudge% = 4
1821                     Mov PScrewPosition(2)
1822                     Break
1823                 EndIf
1824                 Break
1825 '                '
1826             Else
1827                 MCnt% = MFinCnt%
1828                 ScrewGet = 1
1829             EndIf
1830         Else
1831             MCnt% =MFinCnt%
1832         EndIf
1833     Next  MCnt%
1834         '
1835 '    If MScrewJudge% = 0 Then
1836 '        Ovrd 100
1837 '        Spd M_NSpd
1838 '        PScrewPosition(1)
1839 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1840 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1841 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1842 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1843 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1844 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1845 '        'Mov PScrewPosition(2)
1846 '        '������x�z���m�F�@���̍ŏI臒l
1847 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1848 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1849 '            MScrewJudge% = 4
1850 '            ScrewGet = -3
1851 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1852 '            MScrewJudge% = 1
1853 '            ScrewGet = 1
1854 '        EndIf
1855 '        Break
1856 '    EndIf
1857     '
1858 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1859     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1860     '
1861     Select MScrewJudge%
1862 '        Case 0
1863 ''            fErrorProcess(11,162,163,0) '�ُ�I��
1864 '            MCommentD1001 = 162
1865 '            MCommentD1002 = 96
1866 '            Break
1867         Case 2
1868 '            fErrorProcess(11,63,161,0) '����NG
1869             MCommentD1001 = 63
1870             MCommentD1002 = 96
1871             Break
1872         Case 3
1873 '            fErrorProcess(11,160,164,0) '�닟��
1874             MCommentD1001 = 237
1875             MCommentD1002 = 96
1876             Break
1877         Case 4
1878 '            fErrorProcess(11,94,95,0) '�z��NG
1879             MCommentD1001 = 94
1880             MCommentD1002 = 95
1881             Break
1882     End Select
1883     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1884     '
1885     Select M_20#
1886         Case MAbout%          '��~�������ꂽ�ꍇ
1887             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
1888             Mov PInitialPosition
1889             Break
1890         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1891             Break
1892         Case MNext%           '�p���������ꂽ�ꍇ
1893             M_20# = MClear%     '������
1894             Break
1895         Case MNgProcess%      'NG�������ꂽ�ꍇ
1896             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
1897             Mov PInitialPosition
1898             Break
1899         End Select
1900 *End_ScrewGet
1901     Exit Function
1902 FEnd
1903 '
1904 '��ProgramBankSet
1905 ''' <summary>
1906 ''' �˂����߂��s��(P�^�C�g)
1907 ''' </summary>
1908 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1909 '''<param name="MBankNo">�o���N�ԍ�</param>
1910 '''</returns>
1911 ''' <remarks>
1912 ''' Date   : 2021/10/05 : M.Hayakawa
1913 ''' </remarks>'
1914 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1915 '
1916     MLocalPrgNo% = (MProgramNo% - 1) * 32
1917     MLocalBankNo% = MBankNo% * 4
1918 '
1919     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1920         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1921     Else
1922         MLocalOutNo% = 0
1923     EndIf
1924 '
1925     M_Out8(12240) = MLocalOutNo%
1926     Dly 0.1
1927     Exit Function
1928 FEnd
1929 '
1930 '��fnKEY_WAIT()
1931 ''' <summary>
1932 ''' GOT����̃L�[���͑҂�
1933 ''' </summary>
1934 '''<returns>1�F��~    2�F����
1935 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1936 '''         5�FNG
1937 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1938 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1939 '''</returns>
1940 ''' <remarks>
1941 ''' Date   : 2021/07/07 : M.Hayakawa
1942 ''' </remarks>'
1943 Function M% fnKEY_WAIT()
1944     fnKEY_WAIT = 0
1945     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1946     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1947     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1948     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1949     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1950     Dly 0.2
1951     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1952     MLocalLoopFlg=1
1953     While MLocalLoopFlg=1
1954         If M_In(11345) = 1 Then         '��~   M5345
1955             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1956             fnKEY_WAIT = 1
1957             MLocalLoopFlg=-1
1958             Break
1959         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1960             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1961             fnKEY_WAIT = 2
1962             MLocalLoopFlg=-1
1963             Break
1964         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1965             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1966             fnKEY_WAIT = 3
1967             MLocalLoopFlg=-1
1968             Break
1969         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1970             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1971             fnKEY_WAIT = 4
1972             MLocalLoopFlg=-1
1973             Break
1974         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1975             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1976             fnKEY_WAIT = 5
1977             MLocalLoopFlg=-1
1978             Break
1979             '
1980         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1981             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1982             fnKEY_WAIT = MRobotInit1%
1983             MLocalLoopFlg=-1
1984             Break
1985             '
1986         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1987             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1988             fnKEY_WAIT = MRobotInit2%
1989             MLocalLoopFlg=-1
1990             Break
1991             '
1992         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1993             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1994             fnKEY_WAIT = MRobotInit3%
1995             MLocalLoopFlg=-1
1996             Break
1997             '
1998         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1999             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2000             fnKEY_WAIT = MRobotInit4%
2001             MLocalLoopFlg=-1
2002             Break
2003             '
2004         Else
2005         EndIf
2006     WEnd
2007     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2008     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2009     Exit Function
2010 FEnd
2011 '
2012 '�� fnAUTO_CTL
2013 ''' <summary>
2014 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2015 ''' </summary>
2016 ''' <remarks>
2017 ''' Date   : 2021/07/07 : M.Hayakawa
2018 ''' </remarks>
2019 Function M% fnAUTO_CTL
2020     fnAUTO_CTL = 0
2021     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2022     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2023     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2024     '
2025     If M_Svo=0 Then             '�T�[�{ON�m�F
2026         Servo On
2027     EndIf
2028     Wait M_Svo=1
2029     Exit Function
2030 FEnd
2031 '
2032 '�� fnWindScreenOpen
2033 ''' <summary>
2034 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2035 ''' </summary>
2036 '''<param name="%"></param>
2037 '''<param name="%"></param>
2038 '''<param name="%"></param>
2039 '''<param name="%"></param>
2040 ''' <remarks>
2041 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2042 ''' MWindReSet = 0     ��ʔ�\��
2043 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2044 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2045 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2046 ''' Date   : 2021/07/07 : M.Hayakawa
2047 ''' </remarks>
2048 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2049     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2050         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2051     EndIf
2052     '
2053     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2054         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2055     EndIf
2056     '
2057     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2058        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2059     EndIf
2060     '
2061     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2062     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2063     Dly 0.5
2064     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2065     Exit Function
2066 FEnd
2067 '
2068 '��FnCtlValue2
2069 ''' <summary>
2070 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2071 ''' </summary>
2072 ''' <param name="MCtlNo%"></param>
2073 ''' <remarks>
2074 ''' Date : 2022/04/28 �n��
2075 ''' </remarks>
2076 '''
2077 '''  1�F������       �{�P
2078 '''  2�F�g���n�j��   �{�P
2079 '''  3�F�g���m�f��   �{�P (���g�p)
2080 '''  4�F�z���G���[�� �{�P
2081 ''' 99�F�Ǐ��J�n�M�� OFF
2082 '''
2083 Function M% FnCtlValue2(ByVal MCtlNo%)
2084     FnCtlValue2 = 1
2085     Select MCtlNo%
2086         Case 1        '�������{�P
2087             M_Out(12569) = 0             '�����݊J�n�M��OFF
2088             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2089             MInputQty = M_In16(11600)    '��������M
2090             MInputQty = MInputQty + 1    '�������{�P
2091             M_Out16(12592) = MInputQty   '���������M
2092             M_Out(12569) = 1             '�����݊J�n�M��ON
2093             Break
2094             '
2095         Case 2        '�g���n�j���{�P
2096             M_Out(12569) = 0             '�����݊J�n�M��OFF
2097             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2098             MAssyOkQty = M_In16(11616)   '�g��OK����M
2099             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2100             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2101             M_Out(12569) = 1             '�����݊J�n�M��ON
2102             Break
2103             '
2104         Case 4        '�z���G���[���{�P
2105             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2106             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2107             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2108             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2109             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2110             M_Out(12569) = 1                       '�����݊J�n�M��ON
2111             Break
2112             '
2113         Case 99        '�Ǐ��J�n�M��OFF
2114             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2115             M_Out(12569) = 0        '�����݊J�n�M��OFF
2116             Break
2117             '
2118     End Select
2119     Exit Function
2120 FEnd
2121 '
2122 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2123 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2124 '-------------------------------------------------------------------------------
2125 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2126 '   ����
2127 '       PInspPos()      �F�����ʒu
2128 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2129 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2130 '       MInspCnt%       �F�����ʒu��
2131 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2132 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2133 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2134 '   �߂�l�F����
2135 '       0=�ُ�I���A1=����I��
2136 '
2137 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2138 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2139 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2140 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2141 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2142 '-------------------------------------------------------------------------------
2143     '----- �����ݒ� -----
2144     Cnt 0                                                           '�ړ�����������(�����l=0)
2145     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2146 '    Cnt 1,0.1,0.1
2147     '�ϐ��錾�E������
2148     Def Inte MNum                                                   '�����ԍ�(������1�`)
2149     MNum% = 1                                                       '�����ԍ������l�ݒ�
2150     Def Inte MEndFlg                                                '�����I���t���O
2151     MEndFlg% = 0
2152     '
2153     '����G�ԍ��ݒ�v���E�������s�v��off
2154     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2155     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2156     '�G���[�ԍ��N���A
2157     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2158     M_Out16(MOUT_InspErrNum) = MInspErrNum
2159     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2160     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2161     '
2162     'Insight Ready check?
2163     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2164         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2165         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2166         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2167         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2168         Exit Function
2169     EndIf
2170     '
2171     '�����ʒu���m�F
2172     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2173         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2174         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2175         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2176         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2177         Exit Function
2178     EndIf
2179     '
2180     '
2181     '
2182     '----- ���C������ -----
2183     '�ݒ肳�ꂽ�����ʒu�����̌������s
2184     While( MEndFlg% = 0 )
2185         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2186         MSetGrNumRetryExitFlg = 0
2187         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2188         While( MSetGrNumRetryExitFlg = 0 )
2189         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2190             '
2191             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2192             '
2193             '----- �����O���[�v�ԍ��ݒ� -----
2194             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2195             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2196             '
2197             '�����ʒu�ֈړ��E�ړ������҂�
2198             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2199             Mvs PInspPos( MNum% )                                       '�ړ�
2200             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2201             Dly 0.05                                                    '�ړ�������Delay
2202             '
2203             '�����O���[�v�ԍ��ݒ�I���m�F
2204             M_Timer(1) = 0
2205             MExitFlg = 0
2206             While( MExitFlg = 0 )
2207                 '����G�ݒ萳��I��?
2208                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2209                     MExitFlg = 1
2210                 '
2211                 '����G�ݒ�ُ�I��?
2212                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2213                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2214                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2215                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2216                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2217                     EndIf
2218                     MExitFlg = 1
2219                 '
2220                 'timeout�`�F�b�N
2221                 ElseIf 1000 < M_Timer(1) Then
2222                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2223                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2224                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2225                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2226                     EndIf
2227                     MExitFlg = 1
2228                 EndIf
2229             WEnd
2230             '
2231             '����G�ԍ��ݒ�v��off
2232             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2233             '
2234             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2235             'NG�Ȃ���Δ�����
2236             If MCurrentStepErr = 0 Then
2237                 MSetGrNumRetryExitFlg = 1
2238             Else
2239                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2240                 If MSetGrNumRetryCnt = 0 Then
2241                     MSetGrNumRetryExitFlg = 1
2242                 Else
2243                     'Retry�ց@���̑O��Delay
2244                     Dly 0.5
2245                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2246                 EndIf
2247             EndIf
2248             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2249             '
2250         WEnd
2251         '
2252         '
2253         '
2254         '----- �������s -----
2255         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2256             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2257                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2258                 MInspRetryExitFlg = 0
2259                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2260                 While( MInspRetryExitFlg = 0 )
2261                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2262                     '
2263                     '���������m�F
2264                     MRetryCnt = MRetryCnt - 1
2265                     M_Timer(1) = 0
2266                     MExitFlg = 0
2267                     While( MExitFlg = 0 )
2268                     '���������҂�
2269                         '����OK�I��?
2270                         If M_In( MIN_IS_InspOK% ) = 1  Then
2271                             MJudgeOKFlg = 1                         '����OK�t���OON
2272                             MExitFlg = 1
2273                         '
2274                         '����NG�I��?
2275                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2276                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2277                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2278                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2279                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2280                                 EndIf
2281                             EndIf
2282                             MExitFlg = 1
2283                         '
2284                         '�����ُ�I��(IS timeout)?
2285                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2286                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2287                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2288                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2289                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2290                                 EndIf
2291                             EndIf
2292                             MExitFlg = 1
2293                         '
2294                         'timeout�`�F�b�N
2295                         ElseIf 3000 < M_Timer(1) Then
2296                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2297                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2298                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2299                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2300                                 EndIf
2301                             EndIf
2302                             MExitFlg = 1
2303                         EndIf
2304                     WEnd
2305                     '
2306                     '�����J�n�v��off
2307                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2308                     '
2309                     'OK�Ȃ甲����
2310                     If MJudgeOKFlg = 1 Then
2311                         MInspRetryExitFlg = 1
2312                     Else
2313                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2314                         If MRetryCnt = 0 Then
2315                             MInspRetryExitFlg = 1
2316                         Else
2317                             'Retry�ց@���̑O��Delay
2318                             Dly 0.3
2319                         EndIf
2320                     EndIf
2321                     '
2322                 WEnd
2323             EndIf
2324         EndIf
2325         '
2326         '
2327         '
2328         MNum% = MNum% + 1                                           '����Step+1
2329         '�����I���m�F�@�����I���t���O�Z�b�g
2330         If (MInspCnt% < MNum% ) Then
2331             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2332         EndIf
2333         'NG���������s������
2334         If MInspErrNum <> 0 Then                                    'NG����?
2335             If MNgContinue% <> 1 Then                               'NG���s?
2336                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2337             EndIf
2338         EndIf
2339     WEnd
2340     '
2341     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2342     If 0 < MZAxis% Then
2343         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2344         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2345         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2346         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2347     EndIf
2348     '
2349     '�߂�l�ݒ�
2350     If MInspErrNum = 0 Then
2351         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2352     Else
2353         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2354         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2355         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2356     EndIf
2357     Fine 0 , P
2358     Exit Function
2359 FEnd
2360 '
2361 '��fnAutoScreenComment
2362 ''' <summary>
2363 ''' ���C����ʂ̓���󋵕\��
2364 ''' �R�����gD1005�̐ݒ�
2365 ''' </summary>
2366 '''<param name="McommentD1005%">�R�����gID</param>
2367 ''' <remarks>
2368 ''' Date   : 2021/07/07 : M.Hayakawa
2369 ''' </remarks>
2370 Function fnAutoScreenComment(ByVal McommentD1005%)
2371     M_Out16(12576) = McommentD1005%
2372     Exit Function
2373 FEnd
2374 '
2375 '��fnRoboPosChk
2376 ''' <summary>
2377 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2378 ''' </summary>
2379 '''<param name="MINNumber%">���͔ԍ�</param>
2380 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2381 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2382 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2383 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2384 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2385 ''' <remarks>
2386 ''' Date   : 2021/07/07 : M.Hayakawa
2387 ''' </remarks>
2388 Function M% fnRoboPosChk
2389     fnRoboPosChk = 0
2390     MRet = fnStepRead()
2391     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2392     '�E�B���h��ʐ؊���
2393     If MRBTOpeGroupNo > 5 Then
2394         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2395         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2396         Dly 0.2
2397         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2398         Dly 1.5
2399         '
2400         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2401         '
2402         MLoopFlg% = 1
2403         While MLoopFlg% = 1
2404             '
2405             '
2406             MKeyNumber% = fnKEY_WAIT()
2407             Select MKeyNumber%
2408                 Case Is = MAbout%       '��~
2409                     M_20# = MAbout%
2410                     MLoopFlg% = -1
2411                     Break
2412                 Case Is = MNext%        '����
2413                     'MLoopFlg% = -1
2414                     Break
2415                 Case Is = MContinue%    '�p��
2416                     M_20# = MContinue%
2417                     MLoopFlg% = -1
2418                     Break
2419                 Default
2420                     Break
2421             End Select
2422         WEnd
2423     EndIf
2424     '
2425     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2426         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2427         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2428         Select MRBTOpeGroupNo
2429             Case Is = 5                          '�������Ȃ�
2430                 Break
2431             Case Is = 10                         '�����ʒu�֖߂�
2432                 'Mov PTEST001
2433                 Break
2434             Case Is = 15                         '�����ʒu�֖߂�
2435                 'Mov PTEST002
2436                 Dly 0.5
2437                 'Mov PTEST001
2438                 Dly 0.5
2439                 Break
2440             Default
2441                 Break
2442         End Select
2443         '
2444         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2445         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2446         MRBTOpeGroupNo = 5
2447         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2448         Dly 1.0
2449         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2450         fnRoboPosChk = 1                        '�����ʒu������s
2451         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2452     EndIf
2453     Exit Function
2454 FEnd
2455 '
2456 '��frInCheck
2457 ''' <summary>
2458 ''' �Z���T�[IN�`�F�b�N
2459 ''' </summary>
2460 '''<param name="MINNumber%">���͔ԍ�</param>
2461 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2462 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2463 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2464 ''' <remarks>
2465 ''' Date   : 2021/07/07 : M.Hayakawa
2466 ''' </remarks>
2467 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2468     M_Timer(4) = 0
2469     MloopFlg = 0
2470     While MloopFlg = 0
2471         MCrtTime& = M_Timer(4)
2472         If M_In(MINNumber%) = MCMPFLG% Then
2473             MloopFlg = 1
2474             frInCheck = 1
2475         ElseIf MCrtTime& > MTimeCnt& Then
2476             MloopFlg = 1
2477             frInCheck = 0
2478         EndIf
2479     WEnd
2480     Exit Function
2481 FEnd
2482 '-----------------------------------------------
2483 '
2484 '�˂����ߋ@�ʐM�m�F
2485 '
2486 '-----------------------------------------------
2487 Function M% fScewTcomChk
2488     fScewTcomChk = 0
2489     '�ʐM�m�F���M
2490     M_Out(MOUT_ScwT_ComChk%) = MOn%
2491     '�ʐM�m�F��M�ҋ@
2492     Wait M_In(MIN_ScwT_comOK%) = MOn%
2493     '�ʐM�m�F���M�I��
2494     M_Out(MOUT_ScwT_ComChk%) = MOff%
2495     Exit Function
2496 FEnd
2497 '
2498 '
2499 '-----------------------------------------------
2500 '
2501 '�˂����ߊJ�n���M
2502 '
2503 '-----------------------------------------------
2504 Function M% fScewTStart
2505     fScewTStart = 0
2506     '�˂����ߊJ�n�ҋ@����M
2507     Wait M_In(MIN_ScwT_STRec%) = MOn%
2508     Dly 0.1
2509     '�˂����ߊJ�n��M�𑗐M
2510     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
2511     Exit Function
2512 FEnd
2513 '
2514 '
2515 '-----------------------------------------------
2516 '
2517 '�˂����ߊ�����M
2518 '
2519 '-----------------------------------------------
2520 Function M% fScewTFinish
2521     fScewTFinish = 0
2522     '�˂����ߊ����ҋ@����M
2523     Wait M_In(MIN_ScwT_Fin%) = MOn%
2524     Dly 0.1
2525     '�˂����ߊ�����M�𑗐M
2526     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
2527     Exit Function
2528 FEnd
2529 '
2530 '
2531 '-----------------------------------------------
2532 '
2533 '����xx��~��M
2534 '
2535 '-----------------------------------------------
2536 Function M% fScewTCaseStop(ByVal MCase%())
2537     fScewTCaseStop = 0
2538     '����xx��~����M
2539     Wait M_In(MCase%(1)) = MOn%
2540     Dly 0.1
2541     '����xx��~��M�𑗐M
2542     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
2543     Exit Function
2544 FEnd
2545 '
2546 '-----------------------------------------------
2547 '
2548 '�ĊJ�n��M
2549 '
2550 '-----------------------------------------------
2551 Function M% fScewTReStart()
2552     fScewTReStart = 0
2553     '�ĊJ�n����M
2554     Wait M_In(MIN_ScwT_ReST%) = MOn%
2555     Dly 0.1
2556     '�ĊJ�n��M�𑗐M
2557     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
2558     Exit Function
2559 FEnd
2560 '
2561 '��fErrorProcess
2562 '<summary>
2563 '�G���[����
2564 '</summary>
2565 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2566 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2567 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2568 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2569 '<make>
2570 '2021/11/5 �����V��
2571 '</make>
2572 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2573     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2574     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2575     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2576     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2577 *RETRY_ERR_PROCESS
2578      M_20# = MClear%     '������
2579 '        '�G���[�����L�q
2580         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2581 '        'GOT KEY���͑҂�
2582         MKeyNumber = fnKEY_WAIT()
2583 '        '
2584         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2585             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2586 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2587             Break
2588          '
2589         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2590             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2591 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2592         '
2593         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2594             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2595 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2596          '
2597         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2598             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2599 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2600             Break
2601         '
2602         EndIf
2603         '
2604         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2605         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2606     Exit Function
2607 FEnd
2608 '
2609 '��fnInitialZone
2610 ''' <summary>
2611 ''' ���݈ʒu������ɑҔ����A�����ʒu�ɖ߂�
2612 ''' </summary>
2613 ''' <param name="posNum%">�ړ���̃|�W�V�����ԍ�</param>
2614 ''' <remarks>
2615 ''' Date : 2021/12/2 : M.Hayakawa
2616 ''' Update:2022/06/2 : M.Hayakawa ���H���̔���~���A�ɍ��킹�ĕύX
2617 ''' </remarks>
2618 Function fnInitialZone()
2619     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���]
2620 '
2621     Ovrd 5
2622 ' ���ޔ�
2623     PActive = P_Curr
2624     Pmove = PActive
2625 '
2626     If PActive.X > 580 Then
2627         Pmove.Z =380        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2628     Else
2629         Pmove.Z =500        '��L�ȊO��Z:500�܂Ŏ����グ
2630     EndIf
2631 '
2632     Mvs Pmove
2633     Mov PInitialPosition
2634 ' ���b�N���J��
2635     InitialState()
2636 ' ��U��~
2637     fErrorProcess(20,70,256,0)
2638     Exit Function
2639  FEnd
2640 '
2641 '��InitialState
2642 ''' <summary>
2643 ''' �n���h�A����������ʒu�ɂ���
2644 ''' </summary>
2645 ''' <returns>   0 : OK
2646 '''             1 : NG
2647 ''' </returns>
2648 ''' <remarks>
2649 ''' Date : 2021/12/2 : M.Hayakawa
2650 ''' </remarks>
2651 Function M% InitialState()
2652     InitialState = 0
2653     '
2654     '�ʒu���߉���
2655     M_Out(12264) = 0
2656     M_Out(12265)=1 Dly 0.3                  '�v�b�V������
2657     'Wait M_In(11276)=1                      '�v�b�V���ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2658     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    '�v�b�V���ʒu�ߒ[���o(8/26����)
2659     If MRtn = 0 Then
2660         fErrorProcess(11,234,284,0)
2661         Select M_20#
2662             Case MAbout%                    '��~�������ꂽ�ꍇ
2663                 InitialState = 1
2664                 Break
2665             Case MNgProcess%
2666                 InitialState = 1
2667                 Break
2668             Case MContinue%                 '���g���C�������ꂽ�ꍇ
2669                 M_20# = MClear%
2670                 InitialState = 0
2671                 Break
2672             Case MNext%                     '���ւ������ꂽ�ꍇ
2673                 M_20# = MClear%
2674                 InitialState = 0
2675                 Break
2676         End Select
2677     EndIf
2678     *RETRY_POSITIONING_RESTORE
2679     '
2680     M_Out(12262) = 0
2681     M_Out(12263)=1 Dly 0.3                  '�ʒu���߉���
2682     'Wait M_In(11274)=1                      '�ʒu���ߖߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2683     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[���o(8/26����)
2684     If MRtn = 0 Then
2685         fErrorProcess(11,234,284,0)
2686         Select M_20#
2687             Case MAbout%                    '��~�������ꂽ�ꍇ
2688                 InitialState = 1
2689                 Break
2690             Case MNgProcess%
2691                 InitialState = 1
2692                 Break
2693             Case MContinue%                 '���g���C�������ꂽ�ꍇ
2694                 M_20# = MClear%
2695                 InitialState = 0
2696                 Break
2697             Case MNext%                     '���ւ������ꂽ�ꍇ
2698                 M_20# = MClear%
2699                 InitialState = 0
2700                 Break
2701         End Select
2702     EndIf
2703     Exit Function
2704 FEnd
2705 '
2706 '��fnTorqueCheck
2707 ''' <summary>
2708 ''' �g���N�`�F�b�N����p�̃��C��
2709 ''' </summary>
2710 ''' <remarks>
2711 ''' Date   : 2021/12/21 : H.AJI
2712 ''' </remarks>'
2713 Function M% fnTorqueCheck
2714     '�g���N�`�F�b�N�����M  �����n��~
2715     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2716     '
2717     fnTorqueCheck = 0
2718     Ovrd 20
2719     Mov PInitialPosition              '�����ʒu�ړ�
2720     Ovrd 100
2721     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2722     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2723     Dly 0.2
2724     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2725     '
2726     'M6340  �g���N�`�F�b�N��M
2727     'Dly 5.0
2728     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
2729     Dly 1.0
2730     M_Out(12340) = 0
2731     '
2732     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
2733     '
2734     MLoopFlg = 1
2735     While MLoopFlg = 1
2736         '
2737         Mov PInitialPosition              '�����ʒu�ړ�
2738         '
2739         MKeyNumber = fnKEY_WAIT()
2740         Select MKeyNumber
2741             Case Is = 1           '��~
2742                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
2743                 Dly 1.0
2744                 M_Out(12343) = 0
2745                 Ovrd 20
2746                 Mov PTicketRead_1
2747                 Ovrd 100
2748                 M_20# = 1
2749                 MLoopFlg = -1
2750                 Break
2751             Case Is = 2           '����
2752                 Break
2753             Case Is = 3           '�p��
2754                 Break
2755             Case Is = 4           '�g���N�`�F�b�N�J�n
2756                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
2757                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
2758                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2759                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2760                 MRet = fnMoveTorquePosi()
2761                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
2762                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2763                 Break
2764             Default
2765                 Break
2766         End Select
2767     WEnd
2768     '
2769     '�g���N�`�F�b�N����~���M
2770     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2771     '
2772     '���{�b�g�̈ʒu�����ɖ߂�
2773     '
2774     Exit Function
2775  FEnd
2776  '
2777 '
2778 '
2779 '---------------------------
2780 '
2781 '    ���C����ʂ̕\���A��\���ݒ�
2782 '         �R�����gD1001, D1002, D1003�̐ݒ�
2783 '           MWindReSet = 0     ��ʔ�\��
2784 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2785 '           MWindErrScr = 10    �G���[��� D1001, D1002
2786 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2787 '
2788 '---------------------------
2789 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2790     fnMainScreenOpen = 0
2791     '
2792    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2793         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2794     EndIf
2795     '
2796     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2797         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2798     EndIf
2799     '
2800     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2801         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2802     EndIf
2803     '
2804     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2805     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
2806     Dly 0.5
2807     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
2808     Exit Function
2809 FEnd
2810 '
2811 '��Main
2812 ''' <summary>
2813 ''' �g���N�`�F�b�N������
2814 ''' </summary>
2815 ''' <remarks>
2816 ''' Date   : 2021/12/21 : H.AJI
2817 ''' </remarks>'
2818 Function M% fnMoveTorquePosi
2819      fnMoveTorquePosi = 0
2820      Ovrd 50
2821      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
2822     '
2823     Spd M_NSpd
2824 '-------------      �h���C�o�[RST
2825     M_Out(12240)=0     '�h���C�o�[OFF CCW
2826     M_Out(12241)=0     '�h���C�o�[OFF CW
2827     M_Out(12242)=1     '�h���C�o�[���� C1
2828     M_Out(12243)=1     '�h���C�o�[���� C2
2829     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
2830 '---------------------------------------
2831 '[P-11]
2832 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
2833     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
2834     Dly 0.1
2835 '-----------------------
2836    'Cnt 0                           'Cnt����-2�@�I��
2837 '-----------------------
2838     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
2839     Dly 0.2
2840 '-----------------------
2841     ProgramBankSet(1,3)
2842     M_Out(12241)=0                   '�h���C�o�[OFF  CW
2843     'Dly 0.1
2844 '--------------------------------
2845     Ovrd 40
2846    'Dly 0.1
2847 '--------------------------------  �l�W���ߑ��x�ݒ�
2848     Spd 14                            '���C�h 100-40 100% :Spd 12
2849     Dly 0.1
2850 '--------------------------------
2851 '--------------------------------
2852 '---------------------------------�y�˂����ߓ���z
2853 '
2854     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2855    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2856     Dly 0.3                          '�������҂�
2857    M_Out(12241)=1                   '�h���C�o�[ON  CW
2858 '
2859     Wait M_In(11584)=1                '����/�G���[���o
2860     Dly 0.1
2861     Spd M_NSpd
2862    'Ovrd 20
2863     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2864     Wait M_In(11257)=1                '�l�W����SC
2865 '---------------------------------
2866     Dly 0.1
2867     M_Out(12241)=0                    '�h���C�o�[OFF CW
2868     Dly 0.1
2869     M_Out(12242)=0                    '�h���C�o�[���� C1
2870     Dly 0.1
2871     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2872     Dly 0.1
2873     M_Out(12245)=0                    '�v���O����2���� F1
2874 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2875 '
2876     Mvs PTorqueCheck,-60                       '������mov ����ύX
2877     Dly 0.1
2878 '--------------------------------------------------------------
2879    'Ovrd 80
2880 '--------------------------------------------------------------
2881 '---------------------------------------
2882 '---------------------------------------
2883 '---------------------------------------�G���[���E����
2884    *LBL1
2885    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2886    Mvs ,-100
2887    M_Out(12241)=0     '�h���C�o�[OFF CW
2888    Dly 0.1
2889    M_Out(12242)=0     '�h���C�o�[���� C1
2890    Dly 0.1
2891    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2892    Dly 0.1
2893    M_Out(12245)=0     '�v���O�������� F1
2894 '---------------------------------------
2895 '---------------------------------------
2896 '-------------
2897    'Mov PInitPos19049
2898    Dly 0.1
2899 '
2900 '
2901     Exit Function
2902 FEnd
2903 '
2904 '��Main
2905 ''' <summary>
2906 ''' �g������p�̃��C��
2907 ''' </summary>
2908 ''' <remarks>
2909 ''' Date   : 2021/07/07 : M.Hayakawa
2910 ''' </remarks>'
2911 Function Main
2912     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2913     '
2914     If M_Svo=0 Then
2915         Servo On
2916     EndIf
2917     Wait M_Svo=1
2918 '�g���X�^�[�g���t�����v���p���XON
2919     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2920 '�p�g���C�g����
2921     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2922     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2923     '
2924     M_20# = 0                                   'KEY���͏�����
2925     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
2926     MRet% = 0
2927 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2928     PActive = P_Curr                    '���݈ʒu���擾
2929     MRecoveryPass% = 0
2930     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2931         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2932             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2933             MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2934         EndIf
2935     EndIf
2936     EndIf
2937     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2938         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2939             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2940                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2941             EndIf
2942         EndIf
2943     EndIf
2944     If MRecoveryPass% = 0 Then
2945         fnInitialZone()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2946     EndIf
2947     '
2948     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2949         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2950 '�g���N�`�F�b�N
2951         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2952             MRet% = fnTorqueCheck()
2953             Break
2954         Else
2955 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
2956 '                MRtn = InspInit()               '�摜��������������
2957 '            EndIf
2958             '
2959            M_20# = MClear%                    '������
2960 '�g���J�n
2961             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2962                 MRet% = fnAssyStart()
2963             Else
2964                 M_20# = MPass%
2965             EndIf
2966 '�g���I�����t����
2967             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2968             Wait M_In(11572) = 1            '���t�擾����
2969             Dly 0.1
2970             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2971 '���t�^�[���j�b�g�ւ�OUT
2972             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2973             fnAutoScreenComment(89)         'AUTO��� �g����������
2974             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
2975 'OK/NG�t���O�o��
2976             If M_20# <= 0 Then
2977                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
2978             ElseIf M_20# = MPass% Then
2979                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
2980             EndIf
2981 'PIAS�ɑg������������
2982             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
2983                 If M_20# = MPass% Then
2984                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
2985                 Else
2986                     'KEY���͂�NG�̏ꍇ
2987                     If M_20# = MNgProcess% Then
2988                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
2989                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
2990                         MRet% = fnPiasWrite(MNG%)
2991                        nAssyNgQty = nAssyNgQty + 1
2992                     EndIf
2993                     '
2994                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/07����)
2995                     If M_20# = MAssyOK% Then
2996                             '-----------------------
2997                             'D732 -> D2600 �R�s�[�v��
2998                             M_Out(12566) = 1
2999 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3000                             M_Out(12566) = 0
3001                             '
3002                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3003                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3004                             '��ԍ��ƍ�(PP�͖��g�p�j
3005 '                            MRet% = fnPCBNumberCheck()
3006                         Else
3007                             MRet% = 1
3008                         EndIf
3009                         '
3010                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3011                             If M_20# <> MAbout% Then
3012                                 '�H������OK��������
3013                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3014                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3015                                 MRet% = fnPiasWrite(MOK%)
3016                                 nAssyOkQty = 0
3017                                 nAssyOkQty = nAssyOkQty + 1
3018                             Else
3019                                 nAssyOkQty = nAssyOkQty + 1
3020                             EndIf
3021                         EndIf
3022                     EndIf
3023 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3024 '                    MRet% = fnPiasWrite(MOK%)
3025                 EndIf
3026             Else
3027                 nAssyOkQty = nAssyOkQty + 1
3028             EndIf
3029             '
3030             '�g���I�����t��������
3031             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3032             '�������A�g��OK���A�g��NG��������
3033 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3034             '
3035 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3036 '                '�摜�����I������
3037 '                MRtn = InspQuit()
3038 '            EndIf
3039         EndIf
3040         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3041     EndIf
3042 '�p�g���C�g����
3043     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3044     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3045 'GOT�\��
3046     fnAutoScreenComment(93)  'AUTO��� �H������
3047 FEnd
3048 End
3049 '
3050 '���܂��Ȃ��R�����g
3051 '��΍폜�����
3052 '
3053 '
3054 '
3055 '
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
PScrewPos(1)=(+320.73,-172.58,+395.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(2)=(+320.73,-172.58,+335.34,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+320.73,-172.58,+329.34,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPos(1)=(+180.56,+239.25,+380.00,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(2)=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(10)=(+180.56,+239.25,+338.35,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
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
Pmove=(+343.72,-16.25,+500.00,-180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PInitialPosition=(+250.00,+0.00,+450.00,+180.00,+0.00,+180.00)(7,0)
PScrewSoc1=(+299.62,-64.99,+329.08,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_0=(+299.62,-64.99,+335.08,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_1=(+299.62,-64.99,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2=(+320.95,-24.67,+329.23,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_0=(+320.95,-24.67,+335.23,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_1=(+320.95,-24.67,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3=(+381.00,-24.88,+330.40,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_0=(+381.00,-24.88,+336.40,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_1=(+381.00,-24.88,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4=(+391.22,-85.80,+329.84,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_0=(+391.22,-85.80,+335.84,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_1=(+391.22,-85.80,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5=(+371.00,-153.62,+329.34,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_0=(+371.00,-153.62,+335.34,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_1=(+371.00,-153.62,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6=(+320.73,-172.58,+329.34,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_0=(+320.73,-172.58,+335.34,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_1=(+320.73,-172.58,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupply=(+180.56,+239.41,+338.63,+180.00,+0.00,-120.00)(7,0)
PScrewSupply_1=(+180.56,+239.41,+380.00,+180.00,+0.00,-120.00)(7,0)
PScrewSupply_2=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00)(7,0)
PScrewSupply_9=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00)(7,0)
PSocCheck=(+325.25,-60.87,+444.00,+180.00,-0.01,-180.00)(7,0)
PSocCheck_1=(+325.25,-60.87,+470.00,+180.00,-0.01,-180.00)(7,0)
PSocGet=(+627.82,+106.92,+311.65,-179.93,+0.04,-179.12)(7,0)
PSocGet_1=(+627.82,+106.92,+330.00,-179.93,+0.04,-179.12)(7,0)
PSocGet_2=(+627.82,+106.92,+380.00,-179.93,+0.04,-179.12)(7,0)
PSocPcbRead=(+343.72,-16.25,+435.00,-180.00,+0.00,-180.00)(7,0)
PSocPcbRead_1=(+343.72,-16.25,+480.00,-180.00,+0.00,-180.00)(7,0)
PSocPress=(+391.84,+11.73,+353.15,+180.00,-0.01,-180.00)(7,0)
PSocPress_1=(+391.84,+11.73,+369.00,-180.00,+0.00,+180.00)(7,0)
PSocPress_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PSocSet=(+486.15,-97.80,+349.50,+180.00,-0.04,-179.20)(7,0)
PSocSet_1=(+486.15,-97.80,+361.91,+180.00,-0.04,-179.20)(7,0)
PSocSet_2=(+486.15,-97.80,+380.00,+180.00,-0.04,-179.20)(7,0)
PTicketRead=(+603.00,-149.18,+373.00,-179.99,+0.00,+90.00)(7,0)
PTicketRead_1=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00)(7,0)
PTorqueCheck=(+144.46,-240.78,+340.00,-179.99,-0.01,+90.02)(7,0)
PTorqueCheck_1=(+144.45,-240.80,+360.00,-179.99,+0.00,+90.01)(7,0)
