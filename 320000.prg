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
480     '���͐ݒ�(�ሳ)07/30����
481     M_Out(12266) = 1
482     M_Out(12267) = 0
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
767 '--------------------------------------------
768     '���͌��o(�ሳ��)
769     '22/07/29�ǉ� ����
770 '--------------------------------------------
771 *RE_Pa_OUT
772     If M_20# = MContinue% Then
773     M_Out(12266) = 1
774     M_Out(12267) = 0
775     Dly 0.5
776     M_20# = MClear%
777     EndIf
778     MRtn = frInCheck(11277,1,MSETTIMEOUT05&)     'MDV�p���͌��o(22/07/29����)
779     MRtn2 = frInCheck(11278,0,MSETTIMEOUT05&)    'KA�p���͌��o(ON�ŏオ�肷��)(22/07/29����)
780     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompPaOut
781     If MRtn = 0 Then
782         fErrorProcess(11,200,201,0)
783     ElseIf MRtn2 = 0 Then
784         fErrorProcess(11,200,201,0)
785     EndIf
786     If M_20# = MNext% Then M_20# = MClear%
787     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
788     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
789     If M_20# = MContinue% Then GoTo *RE_Pa_OUT
790 *CompPaOut
791     '
792     Ovrd 40
793     Mvs PSocPress               '�v���X�G���h�[�܂ňړ�
794     Dly 0.5
795     'Wait M_In(11270)=1          '�v���X�V�����_�[�o�[�Z���T�[ON�c����OFF��������R�l�N�^�J�o�[�L
796     MRtn = frInCheck(11270,0,MSETTIMEOUT05&)    '�v���X�V�����_�[�o�[�Z���T�[ON(8/27����)
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
1561     Mov PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
1562     Fine 0.05 , P
1563     Ovrd MOvrdA%
1564     ' �����ݒ�
1565     Accel 100, 10
1566     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
1567     Mvs PScrewPosition(2)
1568     ' �����������ɖ߂�
1569     Accel
1570     ' ����Ovrd�ݒ�
1571 '    Ovrd MOvrdA%
1572     Ovrd 100
1573     ' Spd�ݒ�
1574 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1575     Spd MFeedSpd
1576     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
1577     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1578     Select MScrewType%
1579         Case 1
1580             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1581             ProgramBankSet(1,1)
1582             Break
1583         Case 2
1584             ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1585             ProgramBankSet(3,1)
1586             Break
1587         Case 3
1588             ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1589             ProgramBankSet(1,1)
1590             Break
1591         Case 4
1592             ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1593             ProgramBankSet(1,1)
1594             Break
1595         Case 5
1596             ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1597             ProgramBankSet(1,1)
1598             Break
1599         Case 6
1600             ' S�^�C�g�F�v���O����1�A�o���N4�ɐݒ�
1601             ProgramBankSet(1,4)
1602             Break
1603         Default
1604             ' �v���O����1�A�o���N�Ȃ��ݒ�
1605             ProgramBankSet(0,0)
1606             Break
1607     End Select
1608 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1609      '�h���C�o�[ON�@CW
1610     M_Out(12241)=1
1611     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1612     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
1613     Dly 0.1
1614     Fine 0 , P
1615     Spd M_NSpd
1616     '
1617     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
1618         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1619         Dly 0.1
1620        ' �v���O�����E�o���N����
1621         ProgramBankSet(0,0)
1622         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1623         Mvs PScrewPosition(10),-80
1624         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1625         M_Out(12249)=1 Dly 0.3
1626         MOKNGFlg = -1
1627         ScrewTight = 0
1628     Else
1629          '�h���C�o�[OFF�@CW
1630         M_Out(12241)=0
1631 '        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
1632 '        Select MScrewType%
1633 '            Case 1
1634 '                ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1635 '                ProgramBankSet(1,3)
1636 '                Break
1637 '            Case 2
1638 '                ' P�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1639 '                ProgramBankSet(3,3)
1640 '                Break
1641 '            Case 3
1642 '                ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
1643 '                ProgramBankSet(1,3)
1644 '                Break
1645 '            Case 4
1646 '                ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
1647 '                ProgramBankSet(1,3)
1648 '                Break
1649 '            Case 5
1650 '                ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
1651 '                ProgramBankSet(1,3)
1652 '                Break
1653 '            Default
1654 '                ' �v���O����1�A�o���N�Ȃ��ݒ�
1655 '                ProgramBankSet(0,0)
1656 '                Break
1657 '        End Select
1658 '         '�h���C�o�[ON�@CW
1659 '        Mvs PScrewPosition(10)
1660 '        M_Out(12241)=1
1661 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1662 '        Fine 0 , P
1663 '
1664          '�h���C�o�[OFF�@CW
1665         M_Out(12241)=0
1666        ' �v���O�����E�o���N����
1667         ProgramBankSet(0,0)
1668         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1669         M_Out(12249)=1 Dly 0.3
1670     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1671         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1672        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1673         'Mvs PScrewPosition(10),-80
1674         ScrewTight = 1
1675     EndIf
1676 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1677 '    Ovrd 10
1678 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1679     Ovrd 100
1680     Exit Function
1681 FEnd
1682 '
1683 '��ScrewGet
1684 ''' <summary>
1685 ''' �˂������@����˂��𓾂�
1686 ''' </summary>
1687 '''<param name="%">
1688 '''         PScrewPos(1)    �F�˂�������̂˂����
1689 '''         PScrewPos(2)    �F�˂���������_
1690 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1691 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1692 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1693 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1694 '''</param>
1695 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1696 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1697 '''<returns>����
1698 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1699 '''</returns>
1700 ''' <remarks>
1701 ''' Date   : 2021/07/07 : M.Hayakawa
1702 ''' </remarks>
1703 '''<update>
1704 '''Date    : 2021/11/15 : ����
1705 '''</update>
1706 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1707     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
1708     ScrewGet = 0
1709     MScrewJudge% = 0
1710     '�˂������평������G���[�`�F�b�N
1711 ' ���b��폜
1712     'Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
1713     For MCnt% = 0 To MFinCnt%
1714         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1715         If MRtn = 0 Then
1716             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1717             ScrewGet = -1
1718             MScrewJudge% = 2
1719         EndIf
1720         Ovrd 100
1721         If FeederScrewSensor% <> 0 Then
1722             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1723                 'Ovrd 30
1724                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1725                 'NG�Ƃ��Ă����̊֐����甲����
1726                 ScrewGet = -2
1727                 MScrewJudge% = 3
1728             EndIf
1729         EndIf
1730         Ovrd 100
1731         Spd M_NSpd
1732         If MScrewJudge% = 0 Then
1733     '        ScrewGet = 0
1734             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1735             Dly 0.3
1736             MScrewCnt% = 0
1737             MFinCnt% = 2
1738             fnAutoScreenComment(521)     '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1739             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1740             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1741             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1742             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1743             'Mvs PScrewPosition(10), 1.2
1744            Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
1745             '�r�b�g��](�����ʒu�ύX)
1746             M_Out(Y60_Driver)=1
1747             M_Timer(4) = 0
1748             MloopFlg = 0
1749             MCntTime& = 0
1750             While MloopFlg = 0
1751                 MCrtTime& = M_Timer(4)
1752                 If MCrtTime& >= 180 Then
1753                     MloopFlg = 1
1754                 EndIf
1755             WEnd
1756             M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1757             '�z���m�F
1758             MRtn = 0
1759             MRtn = frInCheck(11271, 1, MSETTIMEOUT01&)
1760 '            '�r�b�g��](�����ʒu�ύX)
1761 '            M_Out(Y60_Driver)=1
1762 '            Dly 0.2
1763             '
1764             JOvrd M_NJovrd
1765             Spd M_NSpd
1766             '�l�W�z���m�F�ʒu�ړ�
1767             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1768             Mvs PScrewPosition(10), -30  ' �l�W�z���m�F�ʒu
1769            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1770             '�r�b�g��]��~
1771             M_Out(Y60_Driver)=0
1772             '
1773 '            If MRtn = 1 Then           '�ŏ���臒l���m�F���Ȃ�
1774                 '1�b�ԃl�W�z���m�F
1775                 MRtn = frInCheck(11272, 1, MSETTIMEOUT01&)
1776 '            EndIf
1777             'MRtn = 0'�����G���[
1778             '�z���G���[�̏ꍇ
1779             '�l�W���˂����Y�ɖ߂�
1780             If MRtn = 0 Then
1781                 Ovrd 30      '2����5�ɕύX
1782                 '�r�b�g��]��~
1783                 M_Out(Y60_Driver)=0
1784                 '�l�W�����@���
1785                 Mvs PScrewPosition(1)
1786                 '�X�ɏ��
1787                 Mov PScrewPosition(1), -140
1788                 '�l�W�̂Ĉʒu
1789                 MRtn = FnCtlValue2(4)          '�z���G���[���{�P  2022/04/28 �n��
1790                 Mov PScrewPosition(9)
1791                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1792                 '�z��OFF
1793                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1794                 Dly 0.2
1795                 '�j��ON
1796                 M_Out(Y6B_VB1)=1 '�^��j��ON
1797                 '�r�b�g��]
1798                 M_Out(Y61_Driver)=1
1799                 Dly 0.5
1800                 '                '
1801                 Ovrd 100
1802                 JOvrd M_NJovrd
1803                 Spd M_NSpd
1804                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1805                 Mov PScrewPosition(9), 10
1806                 Mov PScrewPosition(9)
1807                 Dly 0.1
1808                 Mov PScrewPosition(9), 10
1809                 Mov PScrewPosition(9)
1810                 '
1811                 '�l�W�����҂�
1812                 Wait M_In(11272) = 0
1813                 '�r�b�g��]��~
1814                 M_Out(Y61_Driver)=0
1815                 Dly 0.1
1816                 '�j��OFF
1817                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1818                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1819                 Mov PScrewPosition(1), -140
1820                 Ovrd 100
1821                 Spd M_NSpd
1822                 '�l�W�����@���
1823                 Mvs PScrewPosition(1)
1824 '                '
1825                 ScrewGet = -3
1826                 If MCnt% = MFinCnt% Then
1827                     MScrewJudge% = 4
1828                     Mov PScrewPosition(2)
1829                     Break
1830                 EndIf
1831                 Break
1832 '                '
1833             Else
1834                 MCnt% = MFinCnt%
1835                 ScrewGet = 1
1836             EndIf
1837         Else
1838             MCnt% =MFinCnt%
1839         EndIf
1840     Next  MCnt%
1841         '
1842 '    If MScrewJudge% = 0 Then
1843 '        Ovrd 100
1844 '        Spd M_NSpd
1845 '        PScrewPosition(1)
1846 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1847 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1848 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1849 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1850 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1851 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1852 '        'Mov PScrewPosition(2)
1853 '        '������x�z���m�F�@���̍ŏI臒l
1854 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1855 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1856 '            MScrewJudge% = 4
1857 '            ScrewGet = -3
1858 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1859 '            MScrewJudge% = 1
1860 '            ScrewGet = 1
1861 '        EndIf
1862 '        Break
1863 '    EndIf
1864     '
1865 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1866     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1867     '
1868     Select MScrewJudge%
1869 '        Case 0
1870 ''            fErrorProcess(11,162,163,0) '�ُ�I��
1871 '            MCommentD1001 = 162
1872 '            MCommentD1002 = 96
1873 '            Break
1874         Case 2
1875 '            fErrorProcess(11,63,161,0) '����NG
1876             MCommentD1001 = 63
1877             MCommentD1002 = 96
1878             Break
1879         Case 3
1880 '            fErrorProcess(11,160,164,0) '�닟��
1881             MCommentD1001 = 237
1882             MCommentD1002 = 96
1883             Break
1884         Case 4
1885 '            fErrorProcess(11,94,95,0) '�z��NG
1886             MCommentD1001 = 94
1887             MCommentD1002 = 95
1888             Break
1889     End Select
1890     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1891     '
1892     Select M_20#
1893         Case MAbout%          '��~�������ꂽ�ꍇ
1894             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
1895             Mov PInitialPosition
1896             Break
1897         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1898             Break
1899         Case MNext%           '�p���������ꂽ�ꍇ
1900             M_20# = MClear%     '������
1901             Break
1902         Case MNgProcess%      'NG�������ꂽ�ꍇ
1903             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
1904             Mov PInitialPosition
1905             Break
1906         End Select
1907 *End_ScrewGet
1908     Exit Function
1909 FEnd
1910 '
1911 '��ProgramBankSet
1912 ''' <summary>
1913 ''' �˂����߂��s��(P�^�C�g)
1914 ''' </summary>
1915 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1916 '''<param name="MBankNo">�o���N�ԍ�</param>
1917 '''</returns>
1918 ''' <remarks>
1919 ''' Date   : 2021/10/05 : M.Hayakawa
1920 ''' </remarks>'
1921 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1922 '
1923     MLocalPrgNo% = (MProgramNo% - 1) * 32
1924     MLocalBankNo% = MBankNo% * 4
1925 '
1926     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1927         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1928     Else
1929         MLocalOutNo% = 0
1930     EndIf
1931 '
1932     M_Out8(12240) = MLocalOutNo%
1933     Dly 0.1
1934     Exit Function
1935 FEnd
1936 '
1937 '��fnKEY_WAIT()
1938 ''' <summary>
1939 ''' GOT����̃L�[���͑҂�
1940 ''' </summary>
1941 '''<returns>1�F��~    2�F����
1942 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1943 '''         5�FNG
1944 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1945 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1946 '''</returns>
1947 ''' <remarks>
1948 ''' Date   : 2021/07/07 : M.Hayakawa
1949 ''' </remarks>'
1950 Function M% fnKEY_WAIT()
1951     fnKEY_WAIT = 0
1952     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1953     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1954     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1955     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1956     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1957     Dly 0.2
1958     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1959     MLocalLoopFlg=1
1960     While MLocalLoopFlg=1
1961         If M_In(11345) = 1 Then         '��~   M5345
1962             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1963             fnKEY_WAIT = 1
1964             MLocalLoopFlg=-1
1965             Break
1966         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1967             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1968             fnKEY_WAIT = 2
1969             MLocalLoopFlg=-1
1970             Break
1971         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1972             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1973             fnKEY_WAIT = 3
1974             MLocalLoopFlg=-1
1975             Break
1976         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1977             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1978             fnKEY_WAIT = 4
1979             MLocalLoopFlg=-1
1980             Break
1981         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1982             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1983             fnKEY_WAIT = 5
1984             MLocalLoopFlg=-1
1985             Break
1986             '
1987         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1988             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1989             fnKEY_WAIT = MRobotInit1%
1990             MLocalLoopFlg=-1
1991             Break
1992             '
1993         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1994             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1995             fnKEY_WAIT = MRobotInit2%
1996             MLocalLoopFlg=-1
1997             Break
1998             '
1999         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2000             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2001             fnKEY_WAIT = MRobotInit3%
2002             MLocalLoopFlg=-1
2003             Break
2004             '
2005         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2006             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2007             fnKEY_WAIT = MRobotInit4%
2008             MLocalLoopFlg=-1
2009             Break
2010             '
2011         Else
2012         EndIf
2013     WEnd
2014     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2015     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2016     Exit Function
2017 FEnd
2018 '
2019 '�� fnAUTO_CTL
2020 ''' <summary>
2021 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2022 ''' </summary>
2023 ''' <remarks>
2024 ''' Date   : 2021/07/07 : M.Hayakawa
2025 ''' </remarks>
2026 Function M% fnAUTO_CTL
2027     fnAUTO_CTL = 0
2028     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2029     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2030     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2031     '
2032     If M_Svo=0 Then             '�T�[�{ON�m�F
2033         Servo On
2034     EndIf
2035     Wait M_Svo=1
2036     Exit Function
2037 FEnd
2038 '
2039 '�� fnWindScreenOpen
2040 ''' <summary>
2041 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2042 ''' </summary>
2043 '''<param name="%"></param>
2044 '''<param name="%"></param>
2045 '''<param name="%"></param>
2046 '''<param name="%"></param>
2047 ''' <remarks>
2048 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2049 ''' MWindReSet = 0     ��ʔ�\��
2050 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2051 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2052 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2053 ''' Date   : 2021/07/07 : M.Hayakawa
2054 ''' </remarks>
2055 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2056     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2057         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2058     EndIf
2059     '
2060     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2061         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2062     EndIf
2063     '
2064     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2065        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2066     EndIf
2067     '
2068     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2069     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2070     Dly 0.5
2071     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2072     Exit Function
2073 FEnd
2074 '
2075 '��FnCtlValue2
2076 ''' <summary>
2077 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2078 ''' </summary>
2079 ''' <param name="MCtlNo%"></param>
2080 ''' <remarks>
2081 ''' Date : 2022/04/28 �n��
2082 ''' </remarks>
2083 '''
2084 '''  1�F������       �{�P
2085 '''  2�F�g���n�j��   �{�P
2086 '''  3�F�g���m�f��   �{�P (���g�p)
2087 '''  4�F�z���G���[�� �{�P
2088 ''' 99�F�Ǐ��J�n�M�� OFF
2089 '''
2090 Function M% FnCtlValue2(ByVal MCtlNo%)
2091     FnCtlValue2 = 1
2092     Select MCtlNo%
2093         Case 1        '�������{�P
2094             M_Out(12569) = 0             '�����݊J�n�M��OFF
2095             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2096             MInputQty = M_In16(11600)    '��������M
2097             MInputQty = MInputQty + 1    '�������{�P
2098             M_Out16(12592) = MInputQty   '���������M
2099             M_Out(12569) = 1             '�����݊J�n�M��ON
2100             Break
2101             '
2102         Case 2        '�g���n�j���{�P
2103             M_Out(12569) = 0             '�����݊J�n�M��OFF
2104             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2105             MAssyOkQty = M_In16(11616)   '�g��OK����M
2106             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2107             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2108             M_Out(12569) = 1             '�����݊J�n�M��ON
2109             Break
2110             '
2111         Case 4        '�z���G���[���{�P
2112             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2113             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2114             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2115             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2116             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2117             M_Out(12569) = 1                       '�����݊J�n�M��ON
2118             Break
2119             '
2120         Case 99        '�Ǐ��J�n�M��OFF
2121             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2122             M_Out(12569) = 0        '�����݊J�n�M��OFF
2123             Break
2124             '
2125     End Select
2126     Exit Function
2127 FEnd
2128 '
2129 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2130 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2131 '-------------------------------------------------------------------------------
2132 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2133 '   ����
2134 '       PInspPos()      �F�����ʒu
2135 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2136 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2137 '       MInspCnt%       �F�����ʒu��
2138 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2139 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2140 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2141 '   �߂�l�F����
2142 '       0=�ُ�I���A1=����I��
2143 '
2144 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2145 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2146 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2147 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2148 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2149 '-------------------------------------------------------------------------------
2150     '----- �����ݒ� -----
2151     Cnt 0                                                           '�ړ�����������(�����l=0)
2152     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2153 '    Cnt 1,0.1,0.1
2154     '�ϐ��錾�E������
2155     Def Inte MNum                                                   '�����ԍ�(������1�`)
2156     MNum% = 1                                                       '�����ԍ������l�ݒ�
2157     Def Inte MEndFlg                                                '�����I���t���O
2158     MEndFlg% = 0
2159     '
2160     '����G�ԍ��ݒ�v���E�������s�v��off
2161     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2162     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2163     '�G���[�ԍ��N���A
2164     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2165     M_Out16(MOUT_InspErrNum) = MInspErrNum
2166     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2167     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2168     '
2169     'Insight Ready check?
2170     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2171         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2172         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2173         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2174         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2175         Exit Function
2176     EndIf
2177     '
2178     '�����ʒu���m�F
2179     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2180         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2181         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2182         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2183         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2184         Exit Function
2185     EndIf
2186     '
2187     '
2188     '
2189     '----- ���C������ -----
2190     '�ݒ肳�ꂽ�����ʒu�����̌������s
2191     While( MEndFlg% = 0 )
2192         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2193         MSetGrNumRetryExitFlg = 0
2194         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2195         While( MSetGrNumRetryExitFlg = 0 )
2196         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2197             '
2198             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2199             '
2200             '----- �����O���[�v�ԍ��ݒ� -----
2201             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2202             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2203             '
2204             '�����ʒu�ֈړ��E�ړ������҂�
2205             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2206             Mvs PInspPos( MNum% )                                       '�ړ�
2207             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2208             Dly 0.05                                                    '�ړ�������Delay
2209             '
2210             '�����O���[�v�ԍ��ݒ�I���m�F
2211             M_Timer(1) = 0
2212             MExitFlg = 0
2213             While( MExitFlg = 0 )
2214                 '����G�ݒ萳��I��?
2215                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2216                     MExitFlg = 1
2217                 '
2218                 '����G�ݒ�ُ�I��?
2219                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2220                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2221                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2222                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2223                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2224                     EndIf
2225                     MExitFlg = 1
2226                 '
2227                 'timeout�`�F�b�N
2228                 ElseIf 1000 < M_Timer(1) Then
2229                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2230                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2231                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2232                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2233                     EndIf
2234                     MExitFlg = 1
2235                 EndIf
2236             WEnd
2237             '
2238             '����G�ԍ��ݒ�v��off
2239             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2240             '
2241             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2242             'NG�Ȃ���Δ�����
2243             If MCurrentStepErr = 0 Then
2244                 MSetGrNumRetryExitFlg = 1
2245             Else
2246                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2247                 If MSetGrNumRetryCnt = 0 Then
2248                     MSetGrNumRetryExitFlg = 1
2249                 Else
2250                     'Retry�ց@���̑O��Delay
2251                     Dly 0.5
2252                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2253                 EndIf
2254             EndIf
2255             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2256             '
2257         WEnd
2258         '
2259         '
2260         '
2261         '----- �������s -----
2262         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2263             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2264                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2265                 MInspRetryExitFlg = 0
2266                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2267                 While( MInspRetryExitFlg = 0 )
2268                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2269                     '
2270                     '���������m�F
2271                     MRetryCnt = MRetryCnt - 1
2272                     M_Timer(1) = 0
2273                     MExitFlg = 0
2274                     While( MExitFlg = 0 )
2275                     '���������҂�
2276                         '����OK�I��?
2277                         If M_In( MIN_IS_InspOK% ) = 1  Then
2278                             MJudgeOKFlg = 1                         '����OK�t���OON
2279                             MExitFlg = 1
2280                         '
2281                         '����NG�I��?
2282                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2283                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2284                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2285                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2286                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2287                                 EndIf
2288                             EndIf
2289                             MExitFlg = 1
2290                         '
2291                         '�����ُ�I��(IS timeout)?
2292                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2293                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2294                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2295                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2296                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2297                                 EndIf
2298                             EndIf
2299                             MExitFlg = 1
2300                         '
2301                         'timeout�`�F�b�N
2302                         ElseIf 3000 < M_Timer(1) Then
2303                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2304                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2305                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2306                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2307                                 EndIf
2308                             EndIf
2309                             MExitFlg = 1
2310                         EndIf
2311                     WEnd
2312                     '
2313                     '�����J�n�v��off
2314                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2315                     '
2316                     'OK�Ȃ甲����
2317                     If MJudgeOKFlg = 1 Then
2318                         MInspRetryExitFlg = 1
2319                     Else
2320                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2321                         If MRetryCnt = 0 Then
2322                             MInspRetryExitFlg = 1
2323                         Else
2324                             'Retry�ց@���̑O��Delay
2325                             Dly 0.3
2326                         EndIf
2327                     EndIf
2328                     '
2329                 WEnd
2330             EndIf
2331         EndIf
2332         '
2333         '
2334         '
2335         MNum% = MNum% + 1                                           '����Step+1
2336         '�����I���m�F�@�����I���t���O�Z�b�g
2337         If (MInspCnt% < MNum% ) Then
2338             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2339         EndIf
2340         'NG���������s������
2341         If MInspErrNum <> 0 Then                                    'NG����?
2342             If MNgContinue% <> 1 Then                               'NG���s?
2343                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2344             EndIf
2345         EndIf
2346     WEnd
2347     '
2348     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2349     If 0 < MZAxis% Then
2350         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2351         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2352         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2353         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2354     EndIf
2355     '
2356     '�߂�l�ݒ�
2357     If MInspErrNum = 0 Then
2358         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2359     Else
2360         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2361         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2362         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2363     EndIf
2364     Fine 0 , P
2365     Exit Function
2366 FEnd
2367 '
2368 '��fnAutoScreenComment
2369 ''' <summary>
2370 ''' ���C����ʂ̓���󋵕\��
2371 ''' �R�����gD1005�̐ݒ�
2372 ''' </summary>
2373 '''<param name="McommentD1005%">�R�����gID</param>
2374 ''' <remarks>
2375 ''' Date   : 2021/07/07 : M.Hayakawa
2376 ''' </remarks>
2377 Function fnAutoScreenComment(ByVal McommentD1005%)
2378     M_Out16(12576) = McommentD1005%
2379     Exit Function
2380 FEnd
2381 '
2382 '��fnRoboPosChk
2383 ''' <summary>
2384 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2385 ''' </summary>
2386 '''<param name="MINNumber%">���͔ԍ�</param>
2387 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2388 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2389 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2390 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2391 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2392 ''' <remarks>
2393 ''' Date   : 2021/07/07 : M.Hayakawa
2394 ''' </remarks>
2395 Function M% fnRoboPosChk
2396     fnRoboPosChk = 0
2397     MRet = fnStepRead()
2398     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2399     '�E�B���h��ʐ؊���
2400     If MRBTOpeGroupNo > 5 Then
2401         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2402         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2403         Dly 0.2
2404         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2405         Dly 1.5
2406         '
2407         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2408         '
2409         MLoopFlg% = 1
2410         While MLoopFlg% = 1
2411             '
2412             '
2413             MKeyNumber% = fnKEY_WAIT()
2414             Select MKeyNumber%
2415                 Case Is = MAbout%       '��~
2416                     M_20# = MAbout%
2417                     MLoopFlg% = -1
2418                     Break
2419                 Case Is = MNext%        '����
2420                     'MLoopFlg% = -1
2421                     Break
2422                 Case Is = MContinue%    '�p��
2423                     M_20# = MContinue%
2424                     MLoopFlg% = -1
2425                     Break
2426                 Default
2427                     Break
2428             End Select
2429         WEnd
2430     EndIf
2431     '
2432     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2433         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2434         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2435         Select MRBTOpeGroupNo
2436             Case Is = 5                          '�������Ȃ�
2437                 Break
2438             Case Is = 10                         '�����ʒu�֖߂�
2439                 'Mov PTEST001
2440                 Break
2441             Case Is = 15                         '�����ʒu�֖߂�
2442                 'Mov PTEST002
2443                 Dly 0.5
2444                 'Mov PTEST001
2445                 Dly 0.5
2446                 Break
2447             Default
2448                 Break
2449         End Select
2450         '
2451         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2452         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2453         MRBTOpeGroupNo = 5
2454         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2455         Dly 1.0
2456         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2457         fnRoboPosChk = 1                        '�����ʒu������s
2458         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2459     EndIf
2460     Exit Function
2461 FEnd
2462 '
2463 '��frInCheck
2464 ''' <summary>
2465 ''' �Z���T�[IN�`�F�b�N
2466 ''' </summary>
2467 '''<param name="MINNumber%">���͔ԍ�</param>
2468 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2469 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2470 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2471 ''' <remarks>
2472 ''' Date   : 2021/07/07 : M.Hayakawa
2473 ''' </remarks>
2474 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2475     M_Timer(4) = 0
2476     MloopFlg = 0
2477     While MloopFlg = 0
2478         MCrtTime& = M_Timer(4)
2479         If M_In(MINNumber%) = MCMPFLG% Then
2480             MloopFlg = 1
2481             frInCheck = 1
2482         ElseIf MCrtTime& > MTimeCnt& Then
2483             MloopFlg = 1
2484             frInCheck = 0
2485         EndIf
2486     WEnd
2487     Exit Function
2488 FEnd
2489 '-----------------------------------------------
2490 '
2491 '�˂����ߋ@�ʐM�m�F
2492 '
2493 '-----------------------------------------------
2494 Function M% fScewTcomChk
2495     fScewTcomChk = 0
2496     '�ʐM�m�F���M
2497     M_Out(MOUT_ScwT_ComChk%) = MOn%
2498     '�ʐM�m�F��M�ҋ@
2499     Wait M_In(MIN_ScwT_comOK%) = MOn%
2500     '�ʐM�m�F���M�I��
2501     M_Out(MOUT_ScwT_ComChk%) = MOff%
2502     Exit Function
2503 FEnd
2504 '
2505 '
2506 '-----------------------------------------------
2507 '
2508 '�˂����ߊJ�n���M
2509 '
2510 '-----------------------------------------------
2511 Function M% fScewTStart
2512     fScewTStart = 0
2513     '�˂����ߊJ�n�ҋ@����M
2514     Wait M_In(MIN_ScwT_STRec%) = MOn%
2515     Dly 0.1
2516     '�˂����ߊJ�n��M�𑗐M
2517     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
2518     Exit Function
2519 FEnd
2520 '
2521 '
2522 '-----------------------------------------------
2523 '
2524 '�˂����ߊ�����M
2525 '
2526 '-----------------------------------------------
2527 Function M% fScewTFinish
2528     fScewTFinish = 0
2529     '�˂����ߊ����ҋ@����M
2530     Wait M_In(MIN_ScwT_Fin%) = MOn%
2531     Dly 0.1
2532     '�˂����ߊ�����M�𑗐M
2533     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
2534     Exit Function
2535 FEnd
2536 '
2537 '
2538 '-----------------------------------------------
2539 '
2540 '����xx��~��M
2541 '
2542 '-----------------------------------------------
2543 Function M% fScewTCaseStop(ByVal MCase%())
2544     fScewTCaseStop = 0
2545     '����xx��~����M
2546     Wait M_In(MCase%(1)) = MOn%
2547     Dly 0.1
2548     '����xx��~��M�𑗐M
2549     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
2550     Exit Function
2551 FEnd
2552 '
2553 '-----------------------------------------------
2554 '
2555 '�ĊJ�n��M
2556 '
2557 '-----------------------------------------------
2558 Function M% fScewTReStart()
2559     fScewTReStart = 0
2560     '�ĊJ�n����M
2561     Wait M_In(MIN_ScwT_ReST%) = MOn%
2562     Dly 0.1
2563     '�ĊJ�n��M�𑗐M
2564     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
2565     Exit Function
2566 FEnd
2567 '
2568 '��fErrorProcess
2569 '<summary>
2570 '�G���[����
2571 '</summary>
2572 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2573 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2574 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2575 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2576 '<make>
2577 '2021/11/5 �����V��
2578 '</make>
2579 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2580     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2581     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2582     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2583     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2584 *RETRY_ERR_PROCESS
2585      M_20# = MClear%     '������
2586 '        '�G���[�����L�q
2587         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2588 '        'GOT KEY���͑҂�
2589         MKeyNumber = fnKEY_WAIT()
2590 '        '
2591         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2592             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2593 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2594             Break
2595          '
2596         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2597             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2598 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2599         '
2600         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2601             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2602 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2603          '
2604         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2605             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2606 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2607             Break
2608         '
2609         EndIf
2610         '
2611         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2612         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2613     Exit Function
2614 FEnd
2615 '
2616 '��fnInitialZone
2617 ''' <summary>
2618 ''' ���݈ʒu������ɑҔ����A�����ʒu�ɖ߂�
2619 ''' </summary>
2620 ''' <param name="posNum%">�ړ���̃|�W�V�����ԍ�</param>
2621 ''' <remarks>
2622 ''' Date : 2021/12/2 : M.Hayakawa
2623 ''' Update:2022/06/2 : M.Hayakawa ���H���̔���~���A�ɍ��킹�ĕύX
2624 ''' </remarks>
2625 Function fnInitialZone()
2626     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���]
2627 '
2628     Ovrd 5
2629 ' ���ޔ�
2630     PActive = P_Curr
2631     Pmove = PActive
2632 '
2633     If PActive.X > 580 Then
2634         Pmove.Z =380        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2635     Else
2636         Pmove.Z =500        '��L�ȊO��Z:500�܂Ŏ����グ
2637     EndIf
2638 '
2639     Mvs Pmove
2640     Mov PInitialPosition
2641 ' ���b�N���J��
2642     InitialState()
2643 ' ��U��~
2644     fErrorProcess(20,70,256,0)
2645     Exit Function
2646  FEnd
2647 '
2648 '��InitialState
2649 ''' <summary>
2650 ''' �n���h�A����������ʒu�ɂ���
2651 ''' </summary>
2652 ''' <returns>   0 : OK
2653 '''             1 : NG
2654 ''' </returns>
2655 ''' <remarks>
2656 ''' Date : 2021/12/2 : M.Hayakawa
2657 ''' </remarks>
2658 Function M% InitialState()
2659     InitialState = 0
2660     '
2661     '�ʒu���߉���
2662     M_Out(12264) = 0
2663     M_Out(12265)=1 Dly 0.3                  '�v�b�V������
2664     'Wait M_In(11276)=1                      '�v�b�V���ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2665     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    '�v�b�V���ʒu�ߒ[���o(8/26����)
2666     If MRtn = 0 Then
2667         fErrorProcess(11,234,284,0)
2668         Select M_20#
2669             Case MAbout%                    '��~�������ꂽ�ꍇ
2670                 InitialState = 1
2671                 Break
2672             Case MNgProcess%
2673                 InitialState = 1
2674                 Break
2675             Case MContinue%                 '���g���C�������ꂽ�ꍇ
2676                 M_20# = MClear%
2677                 InitialState = 0
2678                 Break
2679             Case MNext%                     '���ւ������ꂽ�ꍇ
2680                 M_20# = MClear%
2681                 InitialState = 0
2682                 Break
2683         End Select
2684     EndIf
2685     *RETRY_POSITIONING_RESTORE
2686     '
2687     M_Out(12262) = 0
2688     M_Out(12263)=1 Dly 0.3                  '�ʒu���߉���
2689     'Wait M_In(11274)=1                      '�ʒu���ߖߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2690     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[���o(8/26����)
2691     If MRtn = 0 Then
2692         fErrorProcess(11,234,284,0)
2693         Select M_20#
2694             Case MAbout%                    '��~�������ꂽ�ꍇ
2695                 InitialState = 1
2696                 Break
2697             Case MNgProcess%
2698                 InitialState = 1
2699                 Break
2700             Case MContinue%                 '���g���C�������ꂽ�ꍇ
2701                 M_20# = MClear%
2702                 InitialState = 0
2703                 Break
2704             Case MNext%                     '���ւ������ꂽ�ꍇ
2705                 M_20# = MClear%
2706                 InitialState = 0
2707                 Break
2708         End Select
2709     EndIf
2710     Exit Function
2711 FEnd
2712 '
2713 '��fnTorqueCheck
2714 ''' <summary>
2715 ''' �g���N�`�F�b�N����p�̃��C��
2716 ''' </summary>
2717 ''' <remarks>
2718 ''' Date   : 2021/12/21 : H.AJI
2719 ''' </remarks>'
2720 Function M% fnTorqueCheck
2721     '�g���N�`�F�b�N�����M  �����n��~
2722     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2723     '
2724     fnTorqueCheck = 0
2725     Ovrd 20
2726     Mov PInitialPosition              '�����ʒu�ړ�
2727     Ovrd 100
2728     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2729     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2730     Dly 0.2
2731     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2732     '
2733     'M6340  �g���N�`�F�b�N��M
2734     'Dly 5.0
2735     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
2736     Dly 1.0
2737     M_Out(12340) = 0
2738     '
2739     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
2740     '
2741     MLoopFlg = 1
2742     While MLoopFlg = 1
2743         '
2744         Mov PInitialPosition              '�����ʒu�ړ�
2745         '
2746         MKeyNumber = fnKEY_WAIT()
2747         Select MKeyNumber
2748             Case Is = 1           '��~
2749                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
2750                 Dly 1.0
2751                 M_Out(12343) = 0
2752                 Ovrd 20
2753                 Mov PTicketRead_1
2754                 Ovrd 100
2755                 M_20# = 1
2756                 MLoopFlg = -1
2757                 Break
2758             Case Is = 2           '����
2759                 Break
2760             Case Is = 3           '�p��
2761                 Break
2762             Case Is = 4           '�g���N�`�F�b�N�J�n
2763                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
2764                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
2765                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2766                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2767                 MRet = fnMoveTorquePosi()
2768                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
2769                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2770                 Break
2771             Default
2772                 Break
2773         End Select
2774     WEnd
2775     '
2776     '�g���N�`�F�b�N����~���M
2777     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2778     '
2779     '���{�b�g�̈ʒu�����ɖ߂�
2780     '
2781     Exit Function
2782  FEnd
2783  '
2784 '
2785 '
2786 '---------------------------
2787 '
2788 '    ���C����ʂ̕\���A��\���ݒ�
2789 '         �R�����gD1001, D1002, D1003�̐ݒ�
2790 '           MWindReSet = 0     ��ʔ�\��
2791 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2792 '           MWindErrScr = 10    �G���[��� D1001, D1002
2793 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2794 '
2795 '---------------------------
2796 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2797     fnMainScreenOpen = 0
2798     '
2799    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2800         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2801     EndIf
2802     '
2803     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2804         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2805     EndIf
2806     '
2807     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2808         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2809     EndIf
2810     '
2811     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2812     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
2813     Dly 0.5
2814     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
2815     Exit Function
2816 FEnd
2817 '
2818 '��Main
2819 ''' <summary>
2820 ''' �g���N�`�F�b�N������
2821 ''' </summary>
2822 ''' <remarks>
2823 ''' Date   : 2021/12/21 : H.AJI
2824 ''' </remarks>'
2825 Function M% fnMoveTorquePosi
2826      fnMoveTorquePosi = 0
2827      Ovrd 50
2828      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
2829     '
2830     Spd M_NSpd
2831 '-------------      �h���C�o�[RST
2832     M_Out(12240)=0     '�h���C�o�[OFF CCW
2833     M_Out(12241)=0     '�h���C�o�[OFF CW
2834     M_Out(12242)=1     '�h���C�o�[���� C1
2835     M_Out(12243)=1     '�h���C�o�[���� C2
2836     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
2837 '---------------------------------------
2838 '[P-11]
2839 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
2840     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
2841     Dly 0.1
2842 '-----------------------
2843    'Cnt 0                           'Cnt����-2�@�I��
2844 '-----------------------
2845     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
2846     Dly 0.2
2847 '-----------------------
2848     ProgramBankSet(1,3)
2849     M_Out(12241)=0                   '�h���C�o�[OFF  CW
2850     'Dly 0.1
2851 '--------------------------------
2852     Ovrd 40
2853    'Dly 0.1
2854 '--------------------------------  �l�W���ߑ��x�ݒ�
2855     Spd 14                            '���C�h 100-40 100% :Spd 12
2856     Dly 0.1
2857 '--------------------------------
2858 '--------------------------------
2859 '---------------------------------�y�˂����ߓ���z
2860 '
2861     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2862    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2863     Dly 0.3                          '�������҂�
2864    M_Out(12241)=1                   '�h���C�o�[ON  CW
2865 '
2866     Wait M_In(11584)=1                '����/�G���[���o
2867     Dly 0.1
2868     Spd M_NSpd
2869    'Ovrd 20
2870     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2871     Wait M_In(11257)=1                '�l�W����SC
2872 '---------------------------------
2873     Dly 0.1
2874     M_Out(12241)=0                    '�h���C�o�[OFF CW
2875     Dly 0.1
2876     M_Out(12242)=0                    '�h���C�o�[���� C1
2877     Dly 0.1
2878     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2879     Dly 0.1
2880     M_Out(12245)=0                    '�v���O����2���� F1
2881 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2882 '
2883     Mvs PTorqueCheck,-60                       '������mov ����ύX
2884     Dly 0.1
2885 '--------------------------------------------------------------
2886    'Ovrd 80
2887 '--------------------------------------------------------------
2888 '---------------------------------------
2889 '---------------------------------------
2890 '---------------------------------------�G���[���E����
2891    *LBL1
2892    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2893    Mvs ,-100
2894    M_Out(12241)=0     '�h���C�o�[OFF CW
2895    Dly 0.1
2896    M_Out(12242)=0     '�h���C�o�[���� C1
2897    Dly 0.1
2898    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2899    Dly 0.1
2900    M_Out(12245)=0     '�v���O�������� F1
2901 '---------------------------------------
2902 '---------------------------------------
2903 '-------------
2904    'Mov PInitPos19049
2905    Dly 0.1
2906 '
2907 '
2908     Exit Function
2909 FEnd
2910 '
2911 '��Main
2912 ''' <summary>
2913 ''' �g������p�̃��C��
2914 ''' </summary>
2915 ''' <remarks>
2916 ''' Date   : 2021/07/07 : M.Hayakawa
2917 ''' </remarks>'
2918 Function Main
2919     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2920     '
2921     If M_Svo=0 Then
2922         Servo On
2923     EndIf
2924     Wait M_Svo=1
2925 '�g���X�^�[�g���t�����v���p���XON
2926     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2927 '�p�g���C�g����
2928     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2929     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2930     '
2931     M_20# = 0                                   'KEY���͏�����
2932     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
2933     MRet% = 0
2934 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2935     PActive = P_Curr                    '���݈ʒu���擾
2936     MRecoveryPass% = 0
2937     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2938         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2939             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2940             MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2941         EndIf
2942     EndIf
2943     EndIf
2944     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2945         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2946             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2947                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2948             EndIf
2949         EndIf
2950     EndIf
2951     If MRecoveryPass% = 0 Then
2952         fnInitialZone()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2953     EndIf
2954     '
2955     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2956         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2957 '�g���N�`�F�b�N
2958         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2959             MRet% = fnTorqueCheck()
2960             Break
2961         Else
2962 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
2963 '                MRtn = InspInit()               '�摜��������������
2964 '            EndIf
2965             '
2966            M_20# = MClear%                    '������
2967 '�g���J�n
2968             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2969                 MRet% = fnAssyStart()
2970             Else
2971                 M_20# = MPass%
2972             EndIf
2973 '�g���I�����t����
2974             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2975             Wait M_In(11572) = 1            '���t�擾����
2976             Dly 0.1
2977             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2978 '���t�^�[���j�b�g�ւ�OUT
2979             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2980             fnAutoScreenComment(89)         'AUTO��� �g����������
2981             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
2982 'OK/NG�t���O�o��
2983             If M_20# <= 0 Then
2984                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
2985             ElseIf M_20# = MPass% Then
2986                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
2987             EndIf
2988 'PIAS�ɑg������������
2989             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
2990                 If M_20# = MPass% Then
2991                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
2992                 Else
2993                     'KEY���͂�NG�̏ꍇ
2994                     If M_20# = MNgProcess% Then
2995                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
2996                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
2997                         MRet% = fnPiasWrite(MNG%)
2998                        nAssyNgQty = nAssyNgQty + 1
2999                     EndIf
3000                     '
3001                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/07����)
3002                     If M_20# = MAssyOK% Then
3003                             '-----------------------
3004                             'D732 -> D2600 �R�s�[�v��
3005                             M_Out(12566) = 1
3006 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3007                             M_Out(12566) = 0
3008                             '
3009                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3010                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3011                             '��ԍ��ƍ�(PP�͖��g�p�j
3012 '                            MRet% = fnPCBNumberCheck()
3013                         Else
3014                             MRet% = 1
3015                         EndIf
3016                         '
3017                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3018                             If M_20# <> MAbout% Then
3019                                 '�H������OK��������
3020                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3021                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3022                                 MRet% = fnPiasWrite(MOK%)
3023                                 nAssyOkQty = 0
3024                                 nAssyOkQty = nAssyOkQty + 1
3025                             Else
3026                                 nAssyOkQty = nAssyOkQty + 1
3027                             EndIf
3028                         EndIf
3029                     EndIf
3030 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3031 '                    MRet% = fnPiasWrite(MOK%)
3032                 EndIf
3033             Else
3034                 nAssyOkQty = nAssyOkQty + 1
3035             EndIf
3036             '
3037             '�g���I�����t��������
3038             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3039             '�������A�g��OK���A�g��NG��������
3040 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3041             '
3042 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3043 '                '�摜�����I������
3044 '                MRtn = InspQuit()
3045 '            EndIf
3046         EndIf
3047         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3048     EndIf
3049 '�p�g���C�g����
3050     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3051     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3052 'GOT�\��
3053     fnAutoScreenComment(93)  'AUTO��� �H������
3054 FEnd
3055 End
3056 '
3057 '���܂��Ȃ��R�����g
3058 '��΍폜�����
3059 '
3060 '
3061 '
3062 '
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
PGetScrewPos(1)=(+180.56,+239.41,+380.00,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(2)=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(10)=(+180.56,+239.41,+338.63,+180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
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
Pmove=(+370.97,-153.62,+500.00,-179.97,-0.01,+89.99,+0.00,+0.00)(7,0)
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
PSocPress=(+391.84,+11.73,+354.93,-180.00,-0.01,+180.00)(7,0)
PSocPress_1=(+391.84,+11.73,+369.00,-180.00,+0.00,+180.00)(7,0)
PSocPress_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PSocSet=(+486.15,-97.80,+349.50,+180.00,-0.04,-179.20)(7,0)
PSocSet_1=(+486.15,-97.80,+361.91,+180.00,-0.04,-179.20)(7,0)
PSocSet_2=(+486.15,-97.80,+380.00,+180.00,-0.04,-179.20)(7,0)
PTicketRead=(+603.00,-149.18,+373.00,-179.99,+0.00,+90.00)(7,0)
PTicketRead_1=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00)(7,0)
PTorqueCheck=(+144.46,-240.78,+340.00,-179.99,-0.01,+90.02)(7,0)
PTorqueCheck_1=(+144.45,-240.80,+360.00,-179.99,+0.00,+90.01)(7,0)
