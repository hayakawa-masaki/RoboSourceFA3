1 ' ===================================
2 '
3 '  210493500 STEP5 Assy1�v���O����CD�p
4 '
5 ' �쐬�ҁF������T
6 ' �쐬���F2022.07.16
7 ' Ver 01 2022.07.16 FA3 DVD�p���x�[�X�ɃW���C���폜�AFA2�Ƃ̓��ꉻ��
8 ' ===================================
9 '===== <Insight�萔> =====
10 '===== <Insight�ϐ���`> =====
11 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
12 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
13 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
14 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
15 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
16 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
17 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
18 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
19 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
20 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
21 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
22 '
23 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
26 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
27 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
28 '
29 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
30 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
33 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
34 '��Ɨp�ϐ�
35 Def Inte MInspErrNum                '�������s�G���[�ԍ�
36 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
37 Def Inte MRtn                       'Function�߂�l�擾�p
38 Def Inte MRtn2                      'Function�߂�l�擾�p
39 Def Inte MRet3                      'Function�߂�l�擾�p
40 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
41 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
42 Def Inte MovrdA                     '�l�W����Ovrd �ϗp   20191127�ǉ�
43 Def Float MSpdA                     '�l�W����Spd�@�ϗp   20191127�ǉ�
44 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p    20200312�ǉ�'
45 MovrdA% = 20                        '�l�W����Ovrd �ϗp   20191127�ǉ�
46 MSpdA = 800                        '�l�W����Spd�@�ϗp   20191127�ǉ�
47 '===== <Insight�ϐ��ݒ�> =====
48 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
49 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
50 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
51 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
52 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
53 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
54 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
55 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
56 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
57 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
58 'Output Signal
59 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
60 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
61 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
62 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
63 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
64 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
65 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
66 '===== <�d�h���萔> =====
67 '===== <�d�h���ϐ���`> =====
68 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
69 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
70 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
71 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
72 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
73 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
74 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
75 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
76 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
77 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
78 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
79 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
80 Y60_Driver=12240 '�d�h�������v��� CCW
81 Y61_Driver=12241 '�d�h�����v��� CW
82 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
83 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
84 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
85 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
86 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
87 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
88 '�g��2
89 X34_NG1=11268 '�˂�����1�@Read
90 X35_NG2=11269 '�˂�����2�@Read
91 '�g��3
92 X3F_NG1=11279 '�˂�����1�@Read
93 '
94 Dim PScrewPosTemp(10)                                               '�l�W���ߗpFunction�����ϐ�
95 Dim PGetScrewPosTemp(10)                                            '�˂������@����˂��𓾂�Function�����ϐ�
96 Dim PEscapePosi(10)
97 MLoopCnt% = 0'
98 '===== <���{�b�g�萔> =====
99 '===== <���{�b�g�ϐ���`> =====
100 MRBTOpeGroupNo = 0                    '���{�b�g����ԍ�������
101 MCommentD1001 = 0
102 MCommentD1002 = 0
103 MCommentD1003 = 0
104 MScreenNo = 0
105 '
106 MCommentTSU = 0
107 MCommentTSD = 0
108 '�E�B���h��ʔԍ��ݒ�
109 MWindReSet = 0
110 MWindInfoScr = 5
111 MWindErrScr = 10
112 MWindErrScr2 = 11
113 MWindErrScr3 = 13
114 MWindErrScr17 = 17
115 MWindErrScr18 = 18
116 MWindCmmnScr = 20
117 MWindJigRelase19049 = 60
118 MWindJigRelase19050 = 61
119 MWindJigRelase19051 = 62
120 '
121 MClear% = 0        'KEY_�̃N���A
122 MAbout% = 1        'KEY_��~
123 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
124 MContinue% = 3     'KEY_�p�� �ēx����������s��
125 '
126 Def Inte MNgProcess
127 MNgProcess% = 5      'KEY_NG
128 '
129 MAssyOK% = 6       '�g������
130 MPass% = 7         '�H���p�X
131 MPiasNG% = 8       'Pias�m�F������NG
132 '
133 '�������pKEY�ԍ�   '
134 MRobotInit1% = 11  '�����ʒu�p
135 MRobotInit2% = 12  '�����ʒu�p
136 MRobotInit3% = 13  '�����ʒu�p
137 MRobotInit4% = 14  '�����ʒu�p
138 '
139 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
140 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
141 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
142 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
143 '
144 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
145 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
146 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
147 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
148 '
149 MopeNo = 0
150 '
151 MOK% = 1               '�e����p
152 MNG% = 0               '�e����p
153 MTIMEOUT% = -1         '�e����p
154 MJudge% = 0            '������i�[�p
155 '
156 '
157 MRECIVETIME& = 0
158 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
159 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
160 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
161 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
162 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
163 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
164 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
165 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
166 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
167 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
168 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
169 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
170 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
171 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
172 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
173 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
174 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
175 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
176 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
177 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
178 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
179 MIN_PIAS_MyProcessComp% = 11573        '���H����������
180 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
181 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
182 '
183 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
184 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
185 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
186 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
187 '
188 MOUT_PiasAssyResultOK% = 12549    '�g��OK
189 MOUT_PiasAssyResultNG% = 12550    '�g��NG
190 MOUT_PiasAssyResultWr% = 12548    '�H��������������
191 '
192 MIN_PiasProcessNG% = 11559        '�H����������NG
193 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
194 MIN_PiasProcessOK% = 11558        '�H����������OK
195 '
196 MIN_Insight_Use% = 11369               '�摜�m�FON
197 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
198 '
199 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
200 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
201 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
202 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
203 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
204 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
205 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
206 '
207 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
208 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
209 '
210 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
211 '
212 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
213 '
214 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
215 MRtn% = 0
216 MopeNo = 0
217 MRet = 0
218 'MRtn = 0
219 MRet3% = 0
220 '
221 Def Inte MInputQty          '������ ���Z�ϐ�
222 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
223 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
224 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
225 Def Inte nAssyOkQty         '���g�p
226 Def Inte MScrewNo
227 Def Inte MReTry
228 '===== <IO�ϐ���`> =====
229 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
230 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
231 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
232 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
233 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
234 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
235 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
236 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
237 '
238 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
239 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
240 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
241 '
242 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
243 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
244 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
245 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
246 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
247 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
248 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
249 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
250 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
251 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
252 '
253 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
254 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
255 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
256 '
257 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
258 '
259 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
260 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
261 '
262 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
263 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
264 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
265 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
266 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
267 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
268 '
269 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
270 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
271 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
272 '
273 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
274 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
275 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
276 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
277 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
278 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
279 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
280 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u 'Y68_VV1% = 12250��Y68_VV1% = 12248�ɕύX(8/27����)
281 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u'Y6B_VB1% = 12251��Y6B_VB1% = 12250�ɕύX(8/27����)
282 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
283 '
284 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
285 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
286 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
287 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
288 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
289 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
290 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
291 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
292 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
293 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
294 '
295 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
296 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
297 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
298 '
299 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
300 '
301 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
302 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
303 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
304 '
305 '
306 '����
307 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
308 Def Inte MOn                            '�o��=1
309 Def Inte MOff                           '�o��=0
310 '
311 '�˂����ߑ��u_�o�̓A�h���X
312 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
313 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
314 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
315 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
316 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
317 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
318 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
319 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
320 '�˂����ߑ��u_���̓A�h���X
321 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
322 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
323 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
324 Def Inte MIN_ScwT_Case1                 '����1��~����M
325 Def Inte MIN_ScwT_Case2                 '����2��~����M
326 Def Inte MIN_ScwT_Case3                 '����3��~����M
327 Def Inte MIN_ScwT_Case4                 '����4��~����M
328 Def Inte MIN_ScwT_Case5                 '����5��~����M
329 '
330 Def Inte MRetryLimit                    ' ���g���C��
331 Def Inte MRetryCount                    ' ���g���C�J�E���g
332 '
333 Dim MScwT_Case1%(2)               '����1��~�ϐ�
334 Dim MScwT_Case2%(2)               '����2��~�ϐ�
335 Dim MScwT_Case3%(2)               '����3��~�ϐ�
336 Dim MScwT_Case4%(2)               '����4��~�ϐ�
337 Dim MScwT_Case5%(2)               '����5��~�ϐ�
338 '
339 Def Pos PActive                     '�������W�n �ʒu�ϐ� ���݈ʒu
340 Def Pos Pmove                       '�������W�n �ʒu�ϐ� �ړ���
341 Def Inte MRecoveryPass              '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s'
342 '����
343 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
344 MOn% = 1                                 '�o�� = 1
345 MOff% = 0                                '�o�� = 0
346 '
347 '�˂����ߋ@_�A�h���X�ݒ�
348 MOUT_ScwT_ComChk% = 12816               '�ʐM�m�F���M
349 MOUT_ScwT_ST% = 12849                   '�˂����ߊJ�n�𑗐M
350 MOUT_ScwT_ReSTOK% = 12850               '�ĊJ�n��M�𑗐M
351 MOUT_ScwT_FinOK% = 12852                '�˂����ߊ�����M�𑗐M
352 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
353 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
354 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
355 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
356 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
357 '
358 MIN_ScwT_comOK% = 11824                 '�˂����ߑ��u����ԐM
359 MIN_ScwT_STRec% = 11857                 '�˂����ߊJ�n����M
360 MIN_ScwT_ReST% = 11858                  '�ĊJ�n����M
361 MIN_ScwT_Fin% = 11860                   '�˂����ߊ�������M
362 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
363 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
364 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
365 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
366 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
367 '
368 MScwT_Case1%(1) = MIN_ScwT_Case1%
369 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
370 MScwT_Case2%(1) = MIN_ScwT_Case2%
371 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
372 MScwT_Case3%(1) = MIN_ScwT_Case3%
373 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
374 MScwT_Case4%(1) = MIN_ScwT_Case4%
375 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
376 MScwT_Case5%(1) = MIN_ScwT_Case5%
377 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
378 '
379 '
380 PCalcGetMainScrew = (+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00)  'Main�˂������@�̕␳�l
381 PCalcGetFanScrew = (+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)   'Fan�˂������@�̕␳�l
382 '
383 MRetryLimit% = 2
384 '
385 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
386 Function M% fnAssyStart
387     M_20# = MClear%                       '������
388     '�g�ݗ��ĊJ�n
389     '�v���O�������_
390     Ovrd 100
391     ' �����ʒu��ID�`�P�b�g��Ƃ��邽�ߍ폜 9/16 M.Hayakawa
392 '    Mov PInitialPosition        '���_���
393     '�����ʒu��ݒ�
394     PTemp = P_Curr
395     MRtn = 0
396     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
397         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
398             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
399                 MRtn = 1
400                 Break
401             EndIf
402             Break
403         EndIf
404         Break
405     EndIf
406     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
407     If MRtn = 1 Then
408         M_Out(12256) = 1 Dly 0.3            '�ʒu���ߏoON
409         Mov PTicketRead
410         Break
411     Else
412         Mov PInitialPosition
413         M_Out(12256) = 1 Dly 0.3           '�ʒu���ߏoON
414         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
415         Mvs PTicketRead             'ID�ǂ݈ʒu
416         Break
417     EndIf
418     *RE_PUSH
419 '    If M_20# = MContinue% Then M_Out(12257) = 0
420     If M_20# = MContinue% Then M_Out(12256) = 1 Dly 0.3
421     If M_20# = MContinue% Then M_20# = MClear%
422     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
423     If MRtn = 1 Then GoTo *CompPush
424         M_Out(12257) = 1 Dly 0.3    ' Y71 1:�ʒu����CY ����
425         fErrorProcess(11,231,282,0)
426     If M_20# = MNext% Then M_Out(12256) = 1 Dly 0.3 'Y70 1:�ʒu����CY �Œ�
427     If M_20# = MNext% Then M_20# = MClear%
428     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
429     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
430     If M_20# = MContinue% Then GoTo *RE_PUSH
431     *CompPush
432 '
433     *RE_READ
434     If M_20# = MContinue% Then M_20# = MClear%
435 '
436     MRtn = 1                            'MRtn������
437     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
438         MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
439     EndIf
440         '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
441         '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
442 '
443     If MRtn = 1 Then GoTo *CompRead
444     Mvs PTicketRead_1                       ' ��U���ɑҔ� �ǉ� 22/07/16 M,H
445     'fErrorProcess(11,97,25,0)
446     If M_20# = MPass% Then GoTo *AssyEnd    ' ���ւ������ꂽ���̃R�����g�����{�W�����v��ύX 2022/07/20 M.H
447     If M_20# = MNext% Then M_20# = MClear%
448     If M_20# = MAbout% Then GoTo *AssyEnd       ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
449     If M_20# = MNgProcess% Then GoTo *AssyEnd   ' �R�����g�����{�W�����v��ύX 22/07/16 M,H
450     If M_20# = MContinue% Then GoTo *RE_READ
451 '    If M_20# = MNext% Then M_20# = MPass%
452     GoTo *ASSY_ERROR_END
453     *CompRead
454     '
455 '�yMAIN���ID�ǂݍ��݁z
456     *RE_MEIN_CHECK
457     PInspPosition(1) = PMainPcbRead 'MAIN��Ǎ��ʒu
458     MInspGroup%(1) = 2              '����G�ԍ�
459     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
460 '
461     If MRtn = 1 Then GoTo *CompMainCheck
462     fErrorProcess(11,38,25,0)
463     If M_20# = MNext% Then M_20# = MClear%
464     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466     If M_20# = MContinue% Then GoTo *RE_MEIN_CHECK
467     *CompMainCheck
468 ''�yGYRO���ID�ǂݍ��݁z
469 '    *RE_GYRO_CHECK
470 '    PInspPosition(1) = PGyroPcbRead 'GYRO��Ǎ��ʒu
471 '    MInspGroup%(1) = 3              '����G�ԍ�
472 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
473 ''
474 '    If MRtn = 1 Then GoTo *CompGyroCheck
475 '    fErrorProcess(11,38,25,0)
476 '    If M_20# = MNext% Then M_20# = MClear%
477 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
478 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
479 '    If M_20# = MContinue% Then GoTo *RE_GYRO_CHECK
480     *CompGyroCheck
481 '�y���ID�R�s�[�z
482     *RE_PCB_RECORD
483     M_Out(12571) = 1    ' �̈�1 ��ԍ��R�s�[ (D2600-) On
484     Dly 0.1
485 '    M_Out(12572) = 1    ' �̈�2 ��ԍ��R�s�[ (D2612-) On
486     Dly 0.1
487     M_Out(12566) = 1    ' toPLC_��ԍ��R�s�[�v�� On
488 '
489     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��R�s�[���� On
490     If MRtn = 1 Then
491         M_Out(12571) = 0  ' �̈�1 ��ԍ��R�s�[ (D2600-) Off
492         Dly 0.1
493 '        M_Out(12572) = 0  ' �̈�2 ��ԍ��R�s�[ (D2612-) Off
494         Dly 0.1
495         M_Out(12566) = 0  ' toPLC_��ԍ��R�s�[�v�� Off
496 '        GoTo *RE_PCB_COMPAIRE   ' ��ԍ��ƍ��ɃX�L�b�v
497     Else
498         fErrorProcess(11,39,25,0)
499         If M_20# = MNext% Then M_20# = MClear%
500         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
501         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
502         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
503     EndIf
504 '�y���ID�ƍ��i�R�t���j�z
505     MRetryCount% = 0
506     While (MRetryCount% <= MRetryLimit%)
507         *RE_PCB_COMPAIRE
508         M_Out(12557)= 1 ' ��ԍ��ƍ��r�b�gON
509         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��ƍ�OK(M420) On
510         If MRtn = 1 Then
511             M_Out(12557)= 0     ' ��ԍ��ƍ��r�b�gOff
512             ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
513             MRetryCount% = 99
514         Else
515             If MRetryCount% = MRetryLimit% Then
516                 If M_In(11565) = 1 Then
517                     fErrorProcess(11,37,25,0)
518                 Else
519                     fErrorProcess(11,38,25,0)
520                 EndIf
521                 If M_20# = MNext% Then
522                     M_20# = MClear%
523                     ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
524                     MRetryCount% = 99
525                 EndIf
526                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
527                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
528                 If M_20# = MContinue% Then
529                     MRetryCount% = 0
530                 EndIf
531             Else
532                 ' ���g���C�񐔃C���N�������g
533                 MRetryCount% = MRetryCount% + 1
534                 Dly 0.1  ' ���̍H���ƃ^�C�~���O�����炷�ׂ̃f�B���C
535             EndIf
536         EndIf
537     WEnd
538 '
539     *RE_CHECK
540     PInspPosition(1) = PParts1Check '���i1�摜�`�F�b�N�ʒu(MAIN����Ӂj
541     MInspGroup%(1) = 4              '����G�ԍ�
542     PInspPosition(2) = PParts2Check '���i2�摜�`�F�b�N�ʒu�i�w�ʔ��Ӂj
543     MInspGroup%(2) = 5              '����G�ԍ�
544 '    PInspPosition(3) = PParts3Check '���i3�摜�`�F�b�N�ʒu�iSOC����Ӂj
545 '    MInspGroup%(3) = 6              '����G�ԍ�
546 '    PInspPosition(4) = PParts4Check '���i4�摜�`�F�b�N�ʒu�i�����Ӂj
547 '    MInspGroup%(4) = 7              '����G�ԍ�
548     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 2, -1, 1 )  '�摜�����������s
549     If MRtn = 1 Then GoTo *CompCheck
550     fErrorProcess(11,43,23,0)
551     If M_20# = MNext% Then M_20# = MClear%
552     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
553     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
554     If M_20# = MContinue% Then GoTo *RE_CHECK
555     *CompCheck
556     '
557     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
558     '���i�ʒu����(ID�Ǎ���ɕύX 9/16 M.Hayakawa�j
559     *RE_POS
560     If M_20# = MContinue% Then M_20# = MClear%
561     M_Out(12256)=1 Dly 0.3      '�ʒu����CY�pSV�o�[�p���X�o��
562     MRtn = FnCtlValue2(99)       '�Ǐ��J�n�M��OFF  2022/04/28 �n��
563 '
564     'Wait M_In(11266)=1          '�ʒu���ߏo�[���o�C���ɂ��R�����g�A�E�g(8/26����))
565     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
566     If MRtn = 1 Then GoTo *Comp_Pos_1
567     fErrorProcess(11,231,282,0)
568     If M_20# = MNext% Then M_20# = MClear%
569     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
570     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
571     If M_20# = MContinue% Then GoTo *RE_POS
572     *Comp_Pos_1
573     '
574     M_Out(12258)=1 Dly 0.3      '�v�b�V��CY�pSV�o�[�p���X�o��(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
575     M_Out(12260)=1 Dly 0.3      'FAN�N�����v�ߒ[�p���X�o��(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
576     Mov PScrewSupplyMain_1
577 '
578 '    M_Out(12258)=1 Dly 0.3      '�v�b�V��CY�pSV�o�[�p���X�o��
579     'Wait M_In(11268)=1          '�v�b�V���o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
580     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '�v�b�V���o�[���o
581     If MRtn = 1 Then GoTo *Comp_Pos_2
582     fErrorProcess(11,231,282,0)
583     If M_20# = MNext% Then M_20# = MClear%
584     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
585     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
586     If M_20# = MContinue% Then GoTo *RE_POS
587     *Comp_Pos_2
588     '
589 '    M_Out(12260)=1 Dly 0.3      'FAN�N�����v�ߒ[�p���X�o��(���C���˂����ߌ�ɕύX M.Hayakawa)(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
590     'Wait M_In(11270)=1          'FAN�N�����v�ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
591     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'FAN�N�����v�ߒ[���o(8/26����)
592     If MRtn = 1 Then GoTo *Comp_Pos_3
593     fErrorProcess(11,231,282,0)
594     If M_20# = MNext% Then M_20# = MClear%
595     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
596     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
597     If M_20# = MContinue% Then GoTo *RE_POS
598     *Comp_Pos_3
599     '
600     '
601     'Main��̃l�W����
602     'Main��p�l�W�����@�փl�W�����ɍs��
603     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
604     '
605     '*ScrewSupplyMain           '�ꎞ�R�����g�A�E�g(�ȉ�5�s,8/5����)
606 '    Mov PScrewSupplyMain_2      '�l�W�����@���_
607 '    Mov PScrewSupplyMain_1      '�l�W�s�b�N�A�b�v���
608 '    Mvs PScrewSupplyMain        '�l�W�s�b�N�A�b�v
609 '    Mvs PScrewSupplyMain_1      '�l�W�s�b�N�A�b�v���
610 '    Mov PScrewSupplyMain_2      '�l�W�����@���_
611     'Return                     '�ꎞ�R�����g�A�E�g(8/4����)
612     'ScrewPositionDebug_1()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
613     '
614     PGetScrewPosTemp(1) = PScrewSupplyMain_1   '�l�W�s�b�N�A�b�v������(8/26����)
615     PGetScrewPosTemp(2) = PScrewSupplyMain_2   '�l�W�������_����(8/26����)
616     PGetScrewPosTemp(9) = PScrewSupplyMain_9   '�l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
617     PGetScrewPosTemp(10) = PScrewSupplyMain    '�l�W�s�b�N�A�b�v����(8/26����)
618     '
619     *RE_SCREW_GET_1                                '���g���C�p���x��
620     If M_20# = MContinue% Then M_20# = MClear%
621     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
622     If M_20# = MClear% Then GoTo *Comp_Screw_1
623     If M_20# = MNext% Then M_20# = MClear%
624     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
625     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
626     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
627     *Comp_Screw_1
628     '
629     '�@�ԃl�W����
630 '    Mov PScrewMain1_1           '�@���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
631 '    Ovrd 5
632 '    Mvs PScrewMain1             '�@�l�W����
633 '    Ovrd 10
634 '    Mvs PScrewMain1_1           '�@���
635     PScrewPosTemp(1) = PScrewMain1_1    '�l�W1���ߊJ�n�ʒu������(8/26����)
636     PScrewPosTemp(2) = PScrewMain1_0    '�l�W1���ߊJ�n�ʒu����(8/26����)
637     PScrewPosTemp(10) = PScrewMain1     '�l�W1���ߏI���ʒu����(8/26����)
638     M_Out16(12672) = 1                  '�l�W���߈ʒu�ԍ����M
639     MRtn = ScrewTight(PScrewPosTemp,1,10.0)    '�l�W1���߂̎��s(8/26����)
640     M_Out16(12672) = 0                  '�l�W���߈ʒu�ԍ��N���A
641     If MRtn = 1 Then GoTo *CompScrew1
642 '
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
963     fnInitialZone() ' �����ʒu�Ɉړ�
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
1619          '�h���C�o�[OFF�@CW
1620         M_Out(12241)=0
1621        ' �v���O�����E�o���N����
1622         ProgramBankSet(0,0)
1623         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1624         M_Out(12249)=1 Dly 0.3
1625     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1626         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1627        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1628         'Mvs PScrewPosition(10),-80
1629         ScrewTight = 1
1630     EndIf
1631 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1632 '    Ovrd 10
1633 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1634     Ovrd 100
1635     Exit Function
1636 *ScrewEnd
1637 FEnd
1638 '
1639 '��ScrewGet
1640 ''' <summary>
1641 ''' �˂������@����˂��𓾂�
1642 ''' </summary>
1643 '''<param name="%">
1644 '''         PScrewPos(1)    �F�˂�������̂˂����
1645 '''         PScrewPos(2)    �F�˂���������_
1646 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1647 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1648 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1649 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1650 '''</param>
1651 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1652 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1653 '''<returns>����
1654 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1655 '''</returns>
1656 ''' <remarks>
1657 ''' Date   : 2021/07/07 : M.Hayakawa
1658 ''' </remarks>
1659 '''<update>
1660 '''Date    : 2021/11/15 : ����
1661 '''Date    : 2021/02/07 : ���� �O�̂��ߊm�F���폜
1662 '''</update>
1663 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1664     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
1665     ScrewGet = 0
1666     MScrewJudge% = 0
1667     '�˂������평������G���[�`�F�b�N
1668 '
1669     For MCnt% = 0 To MFinCnt%
1670         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1671         If MRtn = 0 Then
1672             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1673             ScrewGet = -1
1674             MScrewJudge% = 2
1675         EndIf
1676         Ovrd 100
1677         If FeederScrewSensor% <> 0 Then
1678             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1679                 'Ovrd 30
1680                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1681                 'NG�Ƃ��Ă����̊֐����甲����
1682                 ScrewGet = -2
1683                 MScrewJudge% = 3
1684             EndIf
1685         EndIf
1686         Ovrd 100
1687         Spd M_NSpd
1688         If MScrewJudge% = 0 Then
1689     '        ScrewGet = 0
1690             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1691 '            Dly 0.3                    ' CD�p�ɔ����R�����g��(FA2�Ƌ��ʉ��j M.H
1692             MScrewCnt% = 0
1693             MFinCnt% = 2
1694             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1695             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1696             '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX4/3����)
1697             M_Out(Y60_Driver)=1
1698             '�r�b�g��]�����ԊĎ��J�n
1699             '
1700             '
1701             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1702             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1703             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1704             'Mvs PScrewPosition(10), 1.2
1705            Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
1706 '            '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX4/3����)
1707 '            M_Out(Y60_Driver)=1
1708 '            '�r�b�g��]�����ԊĎ��J�n
1709             M_Timer(4) = 0
1710             MloopFlg = 0
1711             MCrtTime& = 0
1712            '�r�b�g��]����܂őҋ@
1713             While MloopFlg = 0
1714                 MCrtTime& = M_Timer(4)
1715                 If MCrtTime& >= 180 Then
1716                     MloopFlg = 1
1717                 EndIf
1718             WEnd
1719             '
1720            M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1721             '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX3/30����)
1722 '            M_Out(Y60_Driver)=1
1723 '            Dly 0.2
1724             '�z���ʒu�ɂċz���m�F
1725             MRtn = 0
1726             MRtn = frInCheck(11264, 1, MSETTIMEOUT01&)      '�`�F�b�N�͂��邪�G���[����͂��Ȃ�
1727             '
1728             JOvrd M_NJovrd
1729             Spd M_NSpd
1730             '�l�W�z���m�F�ʒu�ړ�
1731             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1732             Mvs PScrewPosition(10), -30  ' �l�W�z���m�F�ʒu
1733            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1734             '�r�b�g��]��~
1735             M_Out(Y60_Driver)=0
1736             '
1737 '            If MRtn = 1 Then            '�V�[�P���X�ύX�ɂ��R�����g�A�E�g(5/13����)
1738                 '1�b�ԃl�W�z���m�F �n�߂�臒l
1739                 MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1740 '            EndIf                       '�V�[�P���X�ύX�ɂ��R�����g�A�E�g(5/13����)
1741             'MRtn = 0'�����G���[
1742             '�z���G���[�̏ꍇ
1743             '�l�W���˂����Y�ɖ߂�
1744             If MRtn = 0 Then
1745                 Ovrd 30      '2����5�ɕύX'5����30�ɕύX(3/30����)
1746                 '�r�b�g��]��~
1747                 M_Out(Y60_Driver)=0
1748                 '�l�W�����@���
1749                 Mvs PScrewPosition(1)
1750                 '�X�ɏ��
1751                 Mov PScrewPosition(1), -140
1752                 '�l�W�̂Ĉʒu
1753                 If FeederReadyNo% = 11260 Then     '�����@�ʂɋz���G���[�����J�E���g�@2022/05/19 �n��
1754                     MRtn = FnCtlValue2(3)          '�����@�Q�z���G���[���{�P
1755                 Else
1756                     MRtn = FnCtlValue2(4)          '�����@�P�z���G���[���{�P  2022/04/28 �n��
1757                 EndIf
1758                 Mov PScrewPosition(9)
1759                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1760                 '�z��OFF
1761                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1762                 Dly 0.2
1763                 '�j��ON
1764                 M_Out(Y6B_VB1)=1 '�^��j��ON
1765                 '�r�b�g��]
1766                 M_Out(Y61_Driver)=1
1767                 Dly 0.5
1768                 '                '
1769                 Ovrd 100
1770                 JOvrd M_NJovrd
1771                 Spd M_NSpd
1772                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1773                 Mov PScrewPosition(9), 10
1774                 Mov PScrewPosition(9)
1775                 Dly 0.1
1776                 Mov PScrewPosition(9), 10
1777                 Mov PScrewPosition(9)
1778                 '
1779                 '�l�W�����҂�
1780                 Wait M_In(11265) = 0
1781                 '�r�b�g��]��~
1782                 M_Out(Y61_Driver)=0
1783                 Dly 0.1
1784                 '�j��OFF
1785                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1786                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1787                 Mov PScrewPosition(1), -140
1788                 Ovrd 100
1789                 Spd M_NSpd
1790                 '�l�W�����@���
1791                 Mvs PScrewPosition(1)
1792 '                '
1793                 ScrewGet = -3
1794                 If MCnt% = MFinCnt% Then
1795                     MScrewJudge% = 4
1796                     Mov PScrewPosition(2)
1797                     Break
1798                 EndIf
1799                 Break
1800 '                '
1801             Else
1802                 MCnt% = MFinCnt%
1803                 ScrewGet = 1
1804             EndIf
1805         Else
1806             MCnt% =MFinCnt%
1807         EndIf
1808     Next  MCnt%
1809         '
1810 '    If MScrewJudge% = 0 Then
1811 '        Ovrd 100
1812 '        Spd M_NSpd
1813 '        PScrewPosition(1)
1814 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1815 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1816 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1817 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1818 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1819 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1820 '        'Mov PScrewPosition(2)
1821 '        '������x�z���m�F�@���̍ŏI臒l
1822 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1823 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1824 '            MScrewJudge% = 4
1825 '            ScrewGet = -3
1826 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1827 '            MScrewJudge% = 1
1828 '            ScrewGet = 1
1829 '        EndIf
1830 '        Break
1831 '    EndIf
1832     '
1833 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1834     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1835     '
1836     Select MScrewJudge%
1837 '        Case 0
1838 ''            fErrorProcess(11,162,163,0) '�ُ�I��
1839 '            MCommentD1001 = 162
1840 '            MCommentD1002 = 96
1841 '            Break
1842         Case 2
1843 '            fErrorProcess(11,63,161,0) '����NG
1844             MCommentD1001 = 63
1845             MCommentD1002 = 96
1846             Break
1847         Case 3
1848 '            fErrorProcess(11,160,164,0) '�닟��
1849             MCommentD1001 = 237
1850             MCommentD1002 = 96
1851             Break
1852         Case 4
1853 '            fErrorProcess(11,94,95,0) '�z��NG
1854             MCommentD1001 = 94
1855             MCommentD1002 = 95
1856             Break
1857     End Select
1858     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1859     '
1860     Select M_20#
1861         Case MAbout%          '��~�������ꂽ�ꍇ
1862             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
1863             Mov PInitialPosition
1864             Break
1865         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1866             Break
1867         Case MNext%           '�p���������ꂽ�ꍇ
1868             M_20# = MClear%     '������
1869             Break
1870         Case MNgProcess%      'NG�������ꂽ�ꍇ
1871             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
1872             Mov PInitialPosition
1873             Break
1874         End Select
1875 *End_ScrewGet
1876     Exit Function
1877 FEnd
1878 '
1879 '��ProgramBankSet
1880 ''' <summary>
1881 ''' �˂����߂��s��(P�^�C�g)
1882 ''' </summary>
1883 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1884 '''<param name="MBankNo">�o���N�ԍ�</param>
1885 '''</returns>
1886 ''' <remarks>
1887 ''' Date   : 2021/10/05 : M.Hayakawa
1888 ''' </remarks>'
1889 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1890 '
1891     MLocalPrgNo% = (MProgramNo% - 1) * 32
1892     MLocalBankNo% = MBankNo% * 4
1893 '
1894     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1895         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1896     Else
1897         MLocalOutNo% = 0
1898     EndIf
1899 '
1900     M_Out8(12240) = MLocalOutNo%
1901     Dly 0.1
1902     Exit Function
1903 FEnd
1904 '
1905 '��fnKEY_WAIT()
1906 ''' <summary>
1907 ''' GOT����̃L�[���͑҂�
1908 ''' </summary>
1909 '''<returns>1�F��~    2�F����
1910 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1911 '''         5�FNG
1912 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1913 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1914 '''</returns>
1915 ''' <remarks>
1916 ''' Date   : 2021/07/07 : M.Hayakawa
1917 ''' </remarks>'
1918 Function M% fnKEY_WAIT()
1919     fnKEY_WAIT = 0
1920     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1921     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1922     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1923     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1924     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1925     Dly 0.2
1926     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1927     MLocalLoopFlg=1
1928     While MLocalLoopFlg=1
1929         If M_In(11345) = 1 Then         '��~   M5345
1930             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1931             fnKEY_WAIT = 1
1932             MLocalLoopFlg=-1
1933             Break
1934         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1935             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1936             fnKEY_WAIT = 2
1937             MLocalLoopFlg=-1
1938             Break
1939         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1940             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1941             fnKEY_WAIT = 3
1942             MLocalLoopFlg=-1
1943             Break
1944         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1945             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1946             fnKEY_WAIT = 4
1947             MLocalLoopFlg=-1
1948             Break
1949         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1950             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1951             fnKEY_WAIT = 5
1952             MLocalLoopFlg=-1
1953             Break
1954         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1955             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1956             fnKEY_WAIT = MRobotInit1%
1957             MLocalLoopFlg=-1
1958             Break
1959         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1960             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1961             fnKEY_WAIT = MRobotInit2%
1962             MLocalLoopFlg=-1
1963             Break
1964         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1965             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1966             fnKEY_WAIT = MRobotInit3%
1967             MLocalLoopFlg=-1
1968             Break
1969         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1970             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1971             fnKEY_WAIT = MRobotInit4%
1972             MLocalLoopFlg=-1
1973             Break
1974         Else
1975         EndIf
1976     WEnd
1977     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1978     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
1979     Exit Function
1980 FEnd
1981 '
1982 '�� fnAUTO_CTL
1983 ''' <summary>
1984 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
1985 ''' </summary>
1986 ''' <remarks>
1987 ''' Date   : 2021/07/07 : M.Hayakawa
1988 ''' </remarks>
1989 Function M% fnAUTO_CTL
1990     fnAUTO_CTL = 0
1991     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1992     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
1993     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1994     '
1995     If M_Svo=0 Then             '�T�[�{ON�m�F
1996         Servo On
1997     EndIf
1998     Wait M_Svo=1
1999     Exit Function
2000 FEnd
2001 '
2002 '�� fnWindScreenOpen
2003 ''' <summary>
2004 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2005 ''' </summary>
2006 '''<param name="%"></param>
2007 '''<param name="%"></param>
2008 '''<param name="%"></param>
2009 '''<param name="%"></param>
2010 ''' <remarks>
2011 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2012 ''' MWindReSet = 0     ��ʔ�\��
2013 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2014 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2015 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2016 ''' Date   : 2021/07/07 : M.Hayakawa
2017 ''' </remarks>
2018 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2019     If MCommentD1001 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2020         M_Out16(12480) = MCommentD1001      'D1001 �R�����g
2021     EndIf
2022     '
2023     If MCommentD1002 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2024         M_Out16(12496) = MCommentD1002      'D1002 �R�����g
2025     EndIf
2026     '
2027     If MCommentD1003 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2028        M_Out16(12512) = MCommentD1003       'D1003 �R�����g
2029     EndIf
2030     '
2031     M_Out16(12448) = MScreenNo              '��ʔԍ�  M6448   10=�G���[���
2032     M_Out(12363) = 1 Dly 0.5                '�E�B���h��ʐݒ�  M6362
2033     Exit Function
2034 FEnd
2035 '
2036 '��FnCtlValue2
2037 ''' <summary>
2038 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2039 ''' </summary>
2040 ''' <param name="MCtlNo%"></param>
2041 ''' <remarks>
2042 ''' Date : 2022/04/28 �n��
2043 ''' </remarks>
2044 '''
2045 '''  1�F������       �{�P
2046 '''  2�F�g���n�j��   �{�P
2047 '''  3�F�����@�Q�z���G���[�� �{�P�@�@�g��NG����ύX 2022/05/19 �n��
2048 '''  4�F�����@�P�z���G���[�� �{�P
2049 ''' 99�F�Ǐ��J�n�M�� OFF
2050 '''
2051 Function M% FnCtlValue2(ByVal MCtlNo%)
2052     FnCtlValue2 = 1
2053     Select MCtlNo%
2054         Case 1        '�������{�P
2055             M_Out(12569) = 0             '�����݊J�n�M��OFF
2056             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2057             MInputQty = M_In16(11600)    '��������M
2058             MInputQty = MInputQty + 1    '�������{�P
2059             M_Out16(12592) = MInputQty   '���������M
2060             M_Out(12569) = 1             '�����݊J�n�M��ON
2061             Break
2062             '
2063         Case 2        '�g���n�j���{�P
2064             M_Out(12569) = 0             '�����݊J�n�M��OFF
2065             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2066             MAssyOkQty = M_In16(11616)   '�g��OK����M
2067             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2068             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2069             M_Out(12569) = 1             '�����݊J�n�M��ON
2070             Break
2071             '
2072         Case 3        '�����@�Q�z���G���[���{�P
2073             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2074             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2075             MSuctionErrQty = M_In16(11632)         '�����@�Q�z���G���[����M
2076             MSuctionErrQty = MSuctionErrQty + 1    '�����@�Q�z���G���[���{�P
2077             M_Out16(12624) = MSuctionErrQty        '�����@�Q�z���G���[�����M
2078             M_Out(12569) = 1                       '�����݊J�n�M��ON
2079             Break
2080             '
2081         Case 4        '�����@�P�z���G���[���{�P
2082             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2083             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2084             MSuctionErrQty = M_In16(11648)         '�����@�P�z���G���[����M
2085             MSuctionErrQty = MSuctionErrQty + 1    '�����@�P�z���G���[���{�P
2086             M_Out16(12640) = MSuctionErrQty        '�����@�P�z���G���[�����M
2087             M_Out(12569) = 1                       '�����݊J�n�M��ON
2088             Break
2089             '
2090         Case 99        '�Ǐ��J�n�M��OFF
2091             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2092             M_Out(12569) = 0        '�����݊J�n�M��OFF
2093             Break
2094             '
2095     End Select
2096     Exit Function
2097 FEnd
2098 '
2099 '
2100 '��FnScreEroorCord
2101 ''' �d���h���C�o�[�̃G���[�R�[�h���܂߂��R�����g���o���ׂ̃R�����g�ԍ��̍쐬
2102 ''' �V�K�쐬�F2022/05/23 : �n��
2103 '''
2104 Function M% FnScreEroorCord()
2105     MScrewErrorCord% = 0
2106     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2107     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2108     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2109     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2110     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2111     MScrewErrorCord% = MScrewErrorCord% * 10
2112     MScrewErrorCord% = MScrewErrorCord% + 500
2113     FnScreEroorCord = MScrewErrorCord%
2114     Exit Function
2115 FEnd
2116 '
2117 '
2118 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2119 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2120 '-------------------------------------------------------------------------------
2121 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2122 '   ����
2123 '       PInspPos()      �F�����ʒu
2124 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2125 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2126 '       MInspCnt%       �F�����ʒu��
2127 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2128 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2129 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2130 '   �߂�l�F����
2131 '       0=�ُ�I���A1=����I��
2132 '
2133 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2134 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2135 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2136 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2137 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2138 '-------------------------------------------------------------------------------
2139     '----- �����ݒ� -----
2140     Cnt 0                                                           '�ړ�����������(�����l=0)
2141     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2142 '    Cnt 1,0.1,0.1
2143     '�ϐ��錾�E������
2144     Def Inte MNum                                                   '�����ԍ�(������1�`)
2145     MNum% = 1                                                       '�����ԍ������l�ݒ�
2146     Def Inte MEndFlg                                                '�����I���t���O
2147     MEndFlg% = 0
2148     '
2149     '����G�ԍ��ݒ�v���E�������s�v��off
2150     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2151     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2152     '�G���[�ԍ��N���A
2153     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2154     M_Out16(MOUT_InspErrNum) = MInspErrNum
2155     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2156     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2157     '
2158     'Insight Ready check?
2159     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2160         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2161         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2162         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2163         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2164         'Exit Function
2165     EndIf
2166     If MInspErrNum = 20 Then GoTo *ISInspectionSingle_End
2167     '
2168     '�����ʒu���m�F
2169     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2170         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2171         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2172         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2173         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2174         'Exit Function
2175     EndIf
2176    If MInspErrNum = 21 Then GoTo *ISInspectionSingle_End
2177     '
2178     '
2179     '
2180     '----- ���C������ -----
2181     '�ݒ肳�ꂽ�����ʒu�����̌������s
2182     While( MEndFlg% = 0 )
2183         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2184         MSetGrNumRetryExitFlg = 0
2185         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2186         While( MSetGrNumRetryExitFlg = 0 )
2187         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2188             '
2189             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2190             '
2191             '----- �����O���[�v�ԍ��ݒ� -----
2192             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2193             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2194             '
2195             '�����ʒu�ֈړ��E�ړ������҂�
2196             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2197             Mov PInspPos( MNum% )                                       '�ړ�
2198             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2199             Dly 0.2                                                     '�ړ�������Delay 0.05>>0.2
2200             '
2201             '�����O���[�v�ԍ��ݒ�I���m�F
2202             M_Timer(1) = 0
2203             MExitFlg = 0
2204             While( MExitFlg = 0 )
2205                 '����G�ݒ萳��I��?
2206                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2207                     MExitFlg = 1
2208                 '
2209                 '����G�ݒ�ُ�I��?
2210                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2211                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2212                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2213                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2214                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2215                     EndIf
2216                     MExitFlg = 1
2217                 '
2218                 'timeout�`�F�b�N
2219                 ElseIf 1000 < M_Timer(1) Then
2220                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2221                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2222                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2223                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2224                     EndIf
2225                     MExitFlg = 1
2226                 EndIf
2227             WEnd
2228             '
2229             '����G�ԍ��ݒ�v��off
2230             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2231             '
2232             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2233             'NG�Ȃ���Δ�����
2234             If MCurrentStepErr = 0 Then
2235                 MSetGrNumRetryExitFlg = 1
2236             Else
2237                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2238                 If MSetGrNumRetryCnt = 0 Then
2239                     MSetGrNumRetryExitFlg = 1
2240                 Else
2241                     'Retry�ց@���̑O��Delay
2242                     Dly 0.5
2243                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2244                 EndIf
2245             EndIf
2246             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2247             '
2248         WEnd
2249         '
2250         '
2251         '
2252         '----- �������s -----
2253         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2254             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2255                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2256                 MInspRetryExitFlg = 0
2257                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2258                 While( MInspRetryExitFlg = 0 )
2259                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2260                     '
2261                     '���������m�F
2262                     MRetryCnt = MRetryCnt - 1
2263                     M_Timer(1) = 0
2264                     MExitFlg = 0
2265                     While( MExitFlg = 0 )
2266                     '���������҂�
2267                         '����OK�I��?
2268                         If M_In( MIN_IS_InspOK% ) = 1  Then
2269                             MJudgeOKFlg = 1                         '����OK�t���OON
2270                             MExitFlg = 1
2271                         '
2272                         '����NG�I��?
2273                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2274                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2275                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2276                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2277                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2278                                 EndIf
2279                             EndIf
2280                             MExitFlg = 1
2281                         '
2282                         '�����ُ�I��(IS timeout)?
2283                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2284                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2285                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2286                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2287                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2288                                 EndIf
2289                             EndIf
2290                             MExitFlg = 1
2291                         '
2292                         'timeout�`�F�b�N
2293                         ElseIf 3000 < M_Timer(1) Then
2294                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2295                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2296                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2297                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2298                                 EndIf
2299                             EndIf
2300                             MExitFlg = 1
2301                         EndIf
2302                     WEnd
2303                     '
2304                     '�����J�n�v��off
2305                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2306                     '
2307                     'OK�Ȃ甲����
2308                     If MJudgeOKFlg = 1 Then
2309                         MInspRetryExitFlg = 1
2310                     Else
2311                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2312                         If MRetryCnt = 0 Then
2313                             MInspRetryExitFlg = 1
2314                         Else
2315                             'Retry�ց@���̑O��Delay
2316                             Dly 0.3
2317                         EndIf
2318                     EndIf
2319                     '
2320                 WEnd
2321             EndIf
2322         EndIf
2323         '
2324         '
2325         '
2326         MNum% = MNum% + 1                                           '����Step+1
2327         '�����I���m�F�@�����I���t���O�Z�b�g
2328         If (MInspCnt% < MNum% ) Then
2329             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2330         EndIf
2331         'NG���������s������
2332         If MInspErrNum <> 0 Then                                    'NG����?
2333             If MNgContinue% <> 1 Then                               'NG���s?
2334                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2335             EndIf
2336         EndIf
2337     WEnd
2338     '
2339     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2340     If 0 < MZAxis% Then
2341         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2342         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2343         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2344         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2345     EndIf
2346     '
2347     '�߂�l�ݒ�
2348     If MInspErrNum = 0 Then
2349         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2350     Else
2351         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2352         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2353         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2354     EndIf
2355 '
2356 *ISInspectionSingle_End
2357 Fine 0 , P
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
2586  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2587             Break
2588          '
2589         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2590             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2591  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2592         '
2593         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2594             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2595  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2596          '
2597         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2598             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2599  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2600             Break
2601         '
2602         EndIf
2603         '
2604         '
2605         '
2606         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2607         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2608     Exit Function
2609 FEnd
2610 '
2611 '��fnInitialZone
2612 ''' <summary>
2613 ''' ���݈ʒu������ɑҔ����A�����ʒu�ɖ߂�
2614 ''' </summary>
2615 ''' <remarks>
2616 ''' Date : 2021/12/2 : M.Hayakawa
2617 ''' Update:2022/06/2 : M.Hayakawa ���H���̔���~���A�ɍ��킹�ĕύX
2618 ''' </remarks>
2619 Function fnInitialZone()
2620     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���]
2621 '
2622     Ovrd 5
2623 ' ���ޔ�
2624     PActive = P_Curr
2625     Pmove = PActive
2626 '
2627     If PActive.X > 580 Then
2628         Pmove.Z =380        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2629     Else
2630         Pmove.Z =500        '��L�ȊO��Z:500�܂Ŏ����グ
2631     EndIf
2632 '
2633     Mvs Pmove
2634     Mov PInitialPosition
2635 ' ���b�N���J��
2636     InitialState()
2637 ' ��U��~
2638     fErrorProcess(20,70,256,0)
2639     Exit Function
2640 FEnd
2641 '
2642 '��InitialState
2643 ''' <summary>
2644 ''' �n���h�A����������ʒu�ɂ���
2645 ''' </summary>
2646 ''' <returns>   0 : OK
2647 '''             1 : NG
2648 ''' </returns>
2649 ''' <remarks>
2650 ''' Date : 2021/12/2 : M.Hayakawa
2651 ''' </remarks>
2652 Function M% InitialState()
2653     InitialState = 0
2654     '���i�ʒu���߉���
2655     M_Out(12261)=1 Dly 0.3      'FAN�N�����v����ON
2656     'Wait M_In(11271)=1          'FAN�N�����v�o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2657     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'FAN�N�����v�o�[���o(8/26����)
2658     If MRtn = 0 Then
2659         fErrorProcess(11,234,284,0)
2660         Select M_20#
2661             Case MAbout%            '��~��
2662                 InitialState = 1
2663                 Break
2664             Case MNgProcess%        'NG�������ꂽ�ꍇ
2665                 InitialState = 0
2666                 Break
2667             Case MContinue%
2668                 M_20# = MClear%
2669                 InitialState = 0
2670                 Break
2671             Case MNext%
2672                 M_20# = MClear%
2673                 InitialState = 0
2674                 Break
2675         End Select
2676     EndIf
2677     '
2678     M_Out(12259)=1 Dly 0.3      '�v�b�V��CY�pSV�ߒ[�p���X�o��
2679     'Wait M_In(11269)=1          '�v�b�V���ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2680     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    '�v�b�V���ߒ[���o
2681     If MRtn = 0 Then
2682         fErrorProcess(11,234,284,0)
2683         Select M_20#
2684             Case MAbout%            '��~��
2685                 InitialState = 1
2686                 Break
2687             Case MNgProcess%        'NG�������ꂽ�ꍇ
2688                 InitialState = 1
2689                 Break
2690             Case MContinue%
2691                 M_20# = MClear%
2692                 InitialState = 0
2693                 Break
2694             Case MNext%
2695                 M_20# = MClear%
2696                 InitialState = 0
2697                 Break
2698         End Select
2699     EndIf
2700     '
2701     M_Out(12257)=1 Dly 0.3      '�ʒu����CY�pSV�ߒ[�p���X�o��
2702     'Wait M_In(11267)=1          '�ʒu���ߖߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2703     MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[���o(8/26����)
2704     If MRtn = 0 Then
2705         fErrorProcess(11,234,284,0)
2706         Select M_20#
2707             Case MAbout%            '��~��
2708                 InitialState = 1
2709                 Break
2710             Case MNgProcess%        'NG�������ꂽ�ꍇ
2711                 InitialState = 1
2712                 Break
2713             Case MContinue%
2714                 M_20# = MClear%
2715                 InitialState = 0
2716                 Break
2717             Case MNext%
2718                 M_20# = MClear%
2719                 InitialState = 0
2720                 Break
2721         End Select
2722     EndIf
2723     Exit Function
2724 FEnd
2725 '
2726 '��fnTorqueCheck
2727 ''' <summary>
2728 ''' �g���N�`�F�b�N����p�̃��C��
2729 ''' </summary>
2730 ''' <remarks>
2731 ''' Date   : 2021/12/21 : H.AJI
2732 ''' </remarks>'
2733 Function M% fnTorqueCheck
2734     '�g���N�`�F�b�N�����M  �����n��~
2735     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2736     '
2737     fnTorqueCheck = 0
2738     Ovrd 20
2739     Mov PInitialPosition              '�����ʒu�ړ�
2740     Ovrd 100
2741     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2742     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2743     Dly 0.2
2744     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2745     '
2746     'M6340  �g���N�`�F�b�N��M
2747     M_Out(12340) = 1 Dly 1.0                '�g���N�`�F�b�N��M M6340
2748     Dly 1.0
2749     M_Out(12340) = 0
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
2761                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
2762                 Dly 1.0
2763                 M_Out(12343) = 0
2764                 Ovrd 20
2765                 Mov PTicketRead_1
2766                 Ovrd 100
2767                 M_20# = 1
2768                 MLoopFlg = -1
2769                 Break
2770             Case Is = 2           '����
2771                 Break
2772             Case Is = 3           '�p��
2773                 Break
2774             Case Is = 4           '�g���N�`�F�b�N�J�n
2775                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
2776                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
2777                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2778                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2779                 MRet = fnMoveTorquePosi()
2780                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
2781                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2782                 Break
2783             Default
2784                 Break
2785         End Select
2786     WEnd
2787     '
2788     '�g���N�`�F�b�N����~���M
2789     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2790     '
2791     '���{�b�g�̈ʒu�����ɖ߂�
2792     '
2793     Exit Function
2794  FEnd
2795  '
2796 '
2797 '
2798 '---------------------------
2799 '
2800 '    ���C����ʂ̕\���A��\���ݒ�
2801 '         �R�����gD1001, D1002, D1003�̐ݒ�
2802 '           MWindReSet = 0     ��ʔ�\��
2803 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2804 '           MWindErrScr = 10    �G���[��� D1001, D1002
2805 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2806 '
2807 '---------------------------
2808 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2809     fnMainScreenOpen = 0
2810     '
2811    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2812         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2813     EndIf
2814     '
2815     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2816         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2817     EndIf
2818     '
2819     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2820         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2821     EndIf
2822     '
2823     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2824     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
2825     Dly 0.5
2826     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
2827     Exit Function
2828 FEnd
2829 '
2830 '��Main
2831 ''' <summary>
2832 ''' �g���N�`�F�b�N������
2833 ''' </summary>
2834 ''' <remarks>
2835 ''' Date   : 2021/12/21 : H.AJI
2836 ''' </remarks>'
2837 Function M% fnMoveTorquePosi
2838      fnMoveTorquePosi = 0
2839      Ovrd 50
2840      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
2841     '
2842     Spd M_NSpd
2843 '-------------      �h���C�o�[RST
2844     M_Out(12240)=0     '�h���C�o�[OFF CCW
2845     M_Out(12241)=0     '�h���C�o�[OFF CW
2846     M_Out(12242)=1     '�h���C�o�[���� C1
2847     M_Out(12243)=1     '�h���C�o�[���� C2
2848     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
2849 '---------------------------------------
2850 '[P-11]
2851 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
2852     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
2853     Dly 0.1
2854 '-----------------------
2855    'Cnt 0                           'Cnt����-2�@�I��
2856 '-----------------------
2857     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
2858     Dly 0.2
2859 '-----------------------
2860     ProgramBankSet(1,3)
2861     M_Out(12241)=0                   '�h���C�o�[OFF  CW
2862     'Dly 0.1
2863 '--------------------------------
2864     Ovrd 40
2865    'Dly 0.1
2866 '--------------------------------  �l�W���ߑ��x�ݒ�
2867     Spd 14                            '���C�h 100-40 100% :Spd 12
2868     Dly 0.1
2869 '--------------------------------
2870 '--------------------------------
2871 '---------------------------------�y�˂����ߓ���z
2872 '
2873     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2874    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2875     Dly 0.3                          '�������҂�
2876    M_Out(12241)=1                   '�h���C�o�[ON  CW
2877 '
2878     Wait M_In(11584)=1                '����/�G���[���o
2879     Dly 0.1
2880     Spd M_NSpd
2881    'Ovrd 20
2882     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2883     Wait M_In(11257)=1                '�l�W����SC
2884 '---------------------------------
2885     Dly 0.1
2886     M_Out(12241)=0                    '�h���C�o�[OFF CW
2887     Dly 0.1
2888     M_Out(12242)=0                    '�h���C�o�[���� C1
2889     Dly 0.1
2890     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2891     Dly 0.1
2892     M_Out(12245)=0                    '�v���O����2���� F1
2893 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2894 '
2895     Mvs PTorqueCheck,-60                       '������mov ����ύX
2896     Dly 0.1
2897 '--------------------------------------------------------------
2898    'Ovrd 80
2899 '--------------------------------------------------------------
2900 '---------------------------------------
2901 '---------------------------------------
2902 '---------------------------------------�G���[���E����
2903    *LBL1
2904    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2905    Mvs ,-100
2906    M_Out(12241)=0     '�h���C�o�[OFF CW
2907    Dly 0.1
2908    M_Out(12242)=0     '�h���C�o�[���� C1
2909    Dly 0.1
2910    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2911    Dly 0.1
2912    M_Out(12245)=0     '�v���O�������� F1
2913 '---------------------------------------
2914 '---------------------------------------
2915 '-------------
2916    'Mov PInitPos19049
2917    Dly 0.1
2918 '
2919 '
2920     Exit Function
2921 FEnd
2922 '
2923 '��Main
2924 ''' <summary>
2925 ''' �g������p�̃��C��
2926 ''' </summary>
2927 ''' <remarks>
2928 ''' Date   : 2021/07/07 : M.Hayakawa
2929 ''' </remarks>'
2930 Function Main
2931     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2932     '
2933     If M_Svo=0 Then
2934         Servo On
2935     EndIf
2936     Wait M_Svo=1
2937 '�g���X�^�[�g���t�����v���p���XON
2938     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2939 '�p�g���C�g����
2940     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2941     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2942     '
2943     M_20# = 0                                   'KEY���͏�����
2944     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
2945     MRet% = 0
2946 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2947     PActive = P_Curr                    '���݈ʒu���擾
2948     MRecoveryPass% = 0
2949     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2950         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2951             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2952             MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2953         EndIf
2954     EndIf
2955     EndIf
2956     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2957         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2958             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2959                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2960             EndIf
2961         EndIf
2962     EndIf
2963     If MRecoveryPass% = 0 Then
2964         fnInitialZone()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2965     EndIf
2966 '
2967     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2968         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2969 '�g���N�`�F�b�N
2970         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2971             MRet% = fnTorqueCheck()
2972             Break
2973         Else
2974 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
2975 '                MRtn = InspInit()               '�摜��������������
2976 '            EndIf
2977             '
2978            M_20# = MClear%                    '������
2979 '�g���J�n
2980             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2981                 fnAssyStart()
2982             Else
2983                 M_20# = MPass%
2984             EndIf
2985 '�g���I�����t����
2986             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2987             Wait M_In(11572) = 1            '���t�擾����
2988             Dly 0.1
2989             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2990             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2991             fnAutoScreenComment(89)         'AUTO��� �g����������
2992 ' ��H���փt���O�o��
2993             If M_20# <> MAbout% Then
2994                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
2995             ElseIf M_20# = MPass% Then
2996                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
2997             EndIf
2998 'About(��~)�ȊO��OK���o�́i�p���b�g�~���j
2999             If M_20# <> MAbout% Then
3000                 M_Out(12339) = 1 Dly 0.5    'M6339  toPLC_RBT�����p���X�o��
3001             EndIf
3002             M_Out(12346) = 0                ' M6346 toPLC_�g���J�n��M OFF
3003 'PIAS�ɑg������������
3004             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3005                 If M_20# = MPass% Then
3006                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3007                 Else
3008                     'KEY���͂�NG�̏ꍇ
3009                     If M_20# = MNgProcess% Then
3010 '                        M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3011                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3012                         MRet% = fnPiasWrite(MNG%)
3013                        nAssyNgQty = nAssyNgQty + 1
3014                     EndIf
3015                     '
3016                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3017                     If M_20# = MAssyOK% Then
3018                             '-----------------------
3019                             'D732 -> D2600 �R�s�[�v��
3020                             M_Out(12566) = 1
3021 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3022                             M_Out(12566) = 0
3023                             '
3024                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3025                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3026                             '��ԍ��ƍ�(PP�͖��g�p�j
3027 '                            MRet% = fnPCBNumberCheck()
3028                         Else
3029                             MRet% = 1
3030                         EndIf
3031                         '
3032                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3033                             If M_20# <> MAbout% Then
3034                                 '�H������OK��������
3035                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3036                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3037                                 MRet% = fnPiasWrite(MOK%)
3038                                 nAssyOkQty = 0
3039                                 nAssyOkQty = nAssyOkQty + 1
3040                             Else
3041                                 nAssyOkQty = nAssyOkQty + 1
3042                             EndIf
3043                         EndIf
3044                     EndIf
3045 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3046 '                    MRet% = fnPiasWrite(MOK%)
3047                 EndIf
3048             Else
3049                 nAssyOkQty = nAssyOkQty + 1
3050             EndIf
3051             '
3052             '�g���I�����t��������
3053             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3054             '�������A�g��OK���A�g��NG��������
3055 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3056             '
3057 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3058 '                '�摜�����I������
3059 '                MRtn = InspQuit()
3060 '            EndIf
3061         EndIf
3062         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3063     EndIf
3064 '�p�g���C�g����
3065     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3066     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3067 'GOT�\��
3068     fnAutoScreenComment(93)  'AUTO��� �H������
3069 '    M_Out(12339) = 1 Dly 0.5        ' M6339 toPLC_RBT�����p���XON
3070 '    M_Out(12346) = 0        'M6346  toPLC_AssY�J�n��M OFF
3071 '
3072 FEnd
3073 End
3074 '
3075 '
3076 '���܂��Ȃ��R�����g
3077 '��΍폜�����
3078 '
3079 '
3080 '
3081 '
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
PGetScrewPosTemp(1)=(+233.52,+389.28,+380.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(2)=(+166.05,+146.93,+400.00,-180.00,+0.00,+128.05,+0.00,+0.00)(7,0)
PGetScrewPosTemp(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPosTemp(9)=(+127.52,+411.68,+432.42,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
PGetScrewPosTemp(10)=(+233.52,+389.28,+338.64,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
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
Pmove=(+602.00,-150.75,+380.00,+180.00,-0.02,+90.00,+0.00,+0.00)(7,0)
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
PScrewSupplyFan=(+233.52,+389.28,+338.64,+180.00,+0.00,+180.00)(7,0)
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
