1 ' ===================================
2 '
3 '  21053001 STEP5 Assy5�v���O����
4 '
5 ' �쐬�ҁFM.Hayakawa
6 ' �쐬���F2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1���痬�p
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
42 Def Inte MovrdA                     '�l�W����Ovrd �ϗp
43 Def Float MSpdA                     '�l�W����Spd�@�ϗp
44 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
45 '===== <Insight�ϐ��ݒ�> =====
46 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
47 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
48 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
49 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
50 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
51 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
52 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
53 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
54 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
55 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
58 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
59 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
60 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
61 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
62 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
63 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
64 '===== <�d�h���ϐ���`> =====
65 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
66 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
67 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
68 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
69 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
70 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
71 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
72 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
73 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
74 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
75 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
76 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
77 Y60_Driver=12240 '�d�h�������v��� CCW
78 Y61_Driver=12241 '�d�h�����v��� CW
79 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
80 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
81 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
82 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
83 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
84 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
85 X34_ScrewReady1=11259 '�˂�����1�@Read
86 '===== <�d�h���萔> =====
87 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
88 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <���{�b�g�萔> =====
92 '===== <���{�b�g�ϐ���`> =====
93 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 '�E�B���h��ʔԍ��ݒ�
102 MWindReSet = 0
103 MWindInfoScr = 5
104 MWindErrScr = 10
105 MWindErrScr2 = 11
106 MWindErrScr3 = 13
107 MWindErrScr17 = 17
108 MWindErrScr18 = 18
109 MWindCmmnScr = 20
110 MWindJigRelase19049 = 60
111 MWindJigRelase19050 = 61
112 MWindJigRelase19051 = 62
113 '
114 MClear% = 0        'KEY_�̃N���A
115 MAbout% = 1        'KEY_��~
116 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
117 MContinue% = 3     'KEY_�p�� �ēx����������s��
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '�g������
123 MPass% = 7         '�H���p�X
124 MPiasNG% = 8       'Pias�m�F������NG
125 '
126 '�������pKEY�ԍ�   '
127 MRobotInit1% = 11  '�����ʒu�p
128 MRobotInit2% = 12  '�����ʒu�p
129 MRobotInit3% = 13  '�����ʒu�p
130 MRobotInit4% = 14  '�����ʒu�p
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
133 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
134 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
135 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
138 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
139 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
140 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
141 '
142 MOK% = 1               '�e����p
143 MNG% = 0               '�e����p
144 MTIMEOUT% = -1         '�e����p
145 MJudge% = 0            '������i�[�p
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
149 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
150 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
151 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
152 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
153 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
154 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
155 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
156 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
157 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
158 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
161 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
162 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
163 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
164 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
165 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
169 MIN_PIAS_MyProcessComp% = 11573        '���H����������
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
171 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
174 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
175 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
176 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
177 '
178 MOUT_PiasAssyResultOK% = 12549    '�g��OK
179 MOUT_PiasAssyResultNG% = 12550    '�g��NG
180 MOUT_PiasAssyResultWr% = 12548    '�H��������������
181 '
182 MIN_PiasProcessNG% = 11559        '�H����������NG
183 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
184 MIN_PiasProcessOK% = 11558        '�H����������OK
185 '
186 MIN_Insight_Use% = 11369               '�摜�m�FON
187 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
191 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
196 '
197 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
198 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
201 '
202 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
203 '
204 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '������ ���Z�ϐ�
211 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
212 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
213 Def Inte MSuctionErrQty     '�z���G���[�� 2022/04/27 �n��
214 Def Inte nAssyOkQty         '���g�p
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO�ϐ���`> =====
218 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
219 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
220 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
221 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
222 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
223 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
224 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
225 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
226 '
227 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
228 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
229 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
230 '
231 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
232 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
233 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
234 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
235 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
236 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
237 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
238 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
239 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
240 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
241 '
242 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
243 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
244 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
245 '
246 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
247 '
248 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
249 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
250 '
251 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
252 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
253 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
254 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
255 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
256 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
261 '
262 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
263 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
264 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
265 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
266 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
267 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
268 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
269 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
270 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
271 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
272 '
273 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
274 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
275 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
276 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
277 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
278 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
279 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
280 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
281 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
282 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
283 '
284 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
285 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
286 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
287 '
288 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
293 '
294 '����
295 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
296 Def Inte MOn                            '�o��=1
297 Def Inte MOff                           '�o��=0
298 '
299 '�˂����ߑ��u_�o�̓A�h���X
300 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
301 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
302 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
303 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
304 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
305 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
307 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
308 '�˂����ߑ��u_���̓A�h���X
309 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
310 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
311 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
312 Def Inte MIN_ScwT_Case1                 '����1��~����M
313 Def Inte MIN_ScwT_Case2                 '����2��~����M
314 Def Inte MIN_ScwT_Case3                 '����3��~����M
315 Def Inte MIN_ScwT_Case4                 '����4��~����M
316 Def Inte MIN_ScwT_Case5                 '����5��~����M
317 '
318 Dim MScwT_Case1%(2)               '����1��~�ϐ�
319 Dim MScwT_Case2%(2)               '����2��~�ϐ�
320 Dim MScwT_Case3%(2)               '����3��~�ϐ�
321 Dim MScwT_Case4%(2)               '����4��~�ϐ�
322 Dim MScwT_Case5%(2)               '����5��~�ϐ�
323 '
324 '����
325 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
326 MOn% = 1                                 '�o�� = 1
327 MOff% = 0                                '�o�� = 0
328 '
329 '�˂����ߋ@_�A�h���X�ݒ�
330 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
331 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
332 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
333 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
334 MOUT_ScwT_Case1OK% = 12874              '����1��~��M�𑗐M
335 MOUT_ScwT_Case2OK% = 12875              '����2��~��M�𑗐M
336 MOUT_ScwT_Case3OK% = 12876              '����3��~��M�𑗐M
337 MOUT_ScwT_Case4OK% = 12877              '����4��~��M�𑗐M
338 MOUT_ScwT_Case5OK% = 12878              '����5��~��M�𑗐M
339 '
340 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
341 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
342 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
343 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
344 MIN_ScwT_Case1% = 11882                 '����1��~�ҋ@����M
345 MIN_ScwT_Case2% = 11883                 '����2��~�ҋ@����M
346 MIN_ScwT_Case3% = 11884                 '����3��~�ҋ@����M
347 MIN_ScwT_Case4% = 11885                 '����4��~�ҋ@����M
348 MIN_ScwT_Case5% = 11886                 '����5��~�ҋ@����M
349 '
350 MScwT_Case1%(1) = MIN_ScwT_Case1%
351 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
352 MScwT_Case2%(1) = MIN_ScwT_Case2%
353 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
354 MScwT_Case3%(1) = MIN_ScwT_Case3%
355 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
356 MScwT_Case4%(1) = MIN_ScwT_Case4%
357 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
358 MScwT_Case5%(1) = MIN_ScwT_Case5%
359 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
360 '
361 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
362 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
363 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
364 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
365 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
366 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
367 Def Inte MRecoveryPass      '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
368 Def Inte MJ6          'J6���̒l���r����ׂ̕ϐ�
369 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
370 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
371 '�����Ӂ������ʒu��ǉ��ύX�������ɂ́A�ύX���K�v�I
372 '
373 'Function�G���[�΍�
374 Def Inte MCtlNo
375 '
376 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
377 Function M% fnAssyStart
378     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
379 '   BaseUnit6�ʐM�m�F
380     *RE_COM_CHECK
381     MRtn = 1    '������
382     If M_In(11920) = 0 Then     'BaseUnit6���E�����̃t���O�𗧂ĂĂ��Ȃ�
383         If M_In(11930) = 0 And M_In(11931) = 0 Then   '�ʐM�ɂĉ�]�p�s��
384             Dly 2.3                                   '��]�҂�
385             If M_In(11930) = 0 And M_In(11931) = 0 Then  '������x�m�F
386                 MRtn = 0                              '�ʐM�ɂĈُ�
387                 Break
388             EndIf
389             Break
390         EndIf
391         Break
392     EndIf
393     If MRtn = 1 Then GoTo *BU6Com_OK    '�ʐMOK�Ȃ烉�x���W�����v
394     fErrorProcess(11,298,284,0)           '0,284��298,287�ɕύX6/7����
395     If M_20# = MNext% Then M_20# = MClear%
396     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
397     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
398     If M_20# = MContinue% Then GoTo *RE_COM_CHECK
399     *BU6Com_OK
400 '
401 '
402 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
403     M_20# = MClear%                       '������
404 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
405 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
406 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
407 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
408 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
409 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
410 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
411 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
412 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
413 '    EndIf
414 '    '
415 '    '���W�ړ�
416 '    '
417 '    '����xx��~
418 '    fScewTCaseStop(MScwT_Case5%)
419 '    '
420 '    '�x�[�X���j�b�gKEY
421 '    Wait M_In(MTEST_KEY%) = MOn%
422 '    '
423 '    '�ĊJ�n
424 '    fScewTReStart()
425 '    '
426 '    '���W�ړ�
427 '    '
428 '    '�˂����ߊ���
429 '    Mret% = fScewTFinish()
430 ' �l�W���߃e�X�g�I��
431 ' PIAS�e�X�g -----------
432 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
433 '    MRet% = fnPiasWrite(MNG%)
434  '   MRet% = fnPCBNumberCheck()
435 ' PIAS�e�X�g�I�� -------
436 '
437     '�g���J�n(9/6�ǉ�(����))
438     '�v���O�������_
439     Ovrd 100
440     ' �n���h��ԏ�����(10/29�ǉ�M.H)(2/11�C��(����))
441     Cmp Off                     '�R���v���C�A���X���[�h�I��
442     ColChk On                   '�Փˌ��mON
443     If M_In(11266) Then
444         M_Out(12256) = 0
445         M_Out(12257) = 1
446     EndIf
447     If M_In(11269) Then
448         M_Out(12258) = 0
449         M_Out(12259) = 1
450     EndIf
451     If M_In(11271) Then
452         M_Out(12260) = 0
453         M_Out(12261) = 1
454     EndIf
455     *WAIT_HAND_INI
456     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
457     *CompHandIni
458     M_Out(12257) = 0
459     M_Out(12259) = 0
460     M_Out(12261) = 0
461 '
462 '
463 'Dly 5                               '�f�o�b�O�p(22/09/30����)
464     ' �˂����ߋ@�e�X�g�p ----------
465     Mret% = fScrewTcomChk()
466     If Mret% = -1 Then GoTo *ASSY_ERROR_END
467     ' �˂����ߋ@�ʐM�J�n
468 '    fScrewTStart()           '�b��R�����g�A�E�g����(10/19����)
469     '�`�P�b�gID��ǂ�
470     PTemp = P_Curr
471     MRtn = 0
472 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
473 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
474 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
475 '                MRtn = 1
476 '            EndIf
477 '        EndIf
478 '    EndIf
479 '    If MRtn = 1 Then
480 '        Mov PTicketRead
481 '    Else
482 '        Cnt 1 , 10 , 10
483 '        Mov PInitialPosition
484 '        Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
485 '        Cnt 0
486 '        Mvs PTicketRead             'ID�ǂ݈ʒu
487 '    EndIf
488 '
489 ' 2022/04/12 ���S�����֏����ύX �n��
490 ' PInitialPosition �ݐ� MStandby=2
491 ' PTicketRead_1 �ݐ� MStandby=1
492 '
493     MStandby = 0    '�ҋ@�ʒu�t���O��������
494     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
495         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
496             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
497                 MStandby = 2
498             EndIf
499         EndIf
500     EndIf
501     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
502         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
503             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
504                 MStandby = 1
505             EndIf
506         EndIf
507     EndIf
508     If MStandby = 2 Then
509         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
510         Cnt 0
511     EndIf
512     If MStandby <> 0 Then GoTo *PositionOK
513     fErrorProcess(11,230,281,0)            '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
514     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
515     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
516     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
517     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
518     *PositionOK
519 '
520     Mvs PTicketRead             'ID�ǂ݈ʒu
521 '
522     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
523     M_Out(12258) = 1            'DVD���J�`���b�N��ON
524 '
525     '
526     MRtn = 1        'MRtn������
527 *RE_TICKET_READ
528 '    MRtn = fnPiasCheck()               'ID�ǂݎ��
529 'PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
530 'MInspGroup%(1) = 1              '����G�ԍ�
531 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
532 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
533     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
534     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
535     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
536 EndIf
537 If MRtn = 1 Then GoTo *CompRead
538     '�G���[�����i�ʒu���߂�����
539 *RE_ERR_REL_1
540 If M_20# = MContinue% Then M_20# = MRtn
541 M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
542 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
543 '
544 If MRtn = 1 Then GoTo *CompErrorRelease
545 MRtn = M_20#        'M_20#�ꎞ���
546 M_20# = MClear%
547 fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
548 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
549 If M_20# = MNext% Then M_20# = MRtn
550 If M_20# = MNgProcess% Then M_20# = MAbout%
551 *CompErrorRelease
552 '
553 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
554 If M_20# = MNext% Then M_20# = MPass%
555 Mvs PTicketRead_1                         '22/04/07 �ǉ� �n��
556 GoTo *ASSY_ERROR_END
557 *CompRead
558 '
559 '    �p���b�g���琻�i�����
560     fScrewTStart()           '�l�W���ߊJ�n
561 '
562     Mov PProductOnPltGet_2      '�{�̎󂯎������_
563     M_Out(12256) = 0            '�{�̃`���b�N��OFF(�����ʒu�ύX2/11����)
564     M_Out(12257) = 1            '�{�̃`���b�N�JON
565 '
566     *RE_POSITIONING
567     '
568     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
569 '    Wait M_In(11273) = 1     '�{�̈ʒu���ߏo�[���o
570     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '�{�̈ʒu���ߏo�[���o
571     If MRtn = 1 Then GoTo *CompPositioning
572     M_Out(12263) = 1 Dly 0.5
573     fErrorProcess(11,231,282,0)
574     If M_20# = MNext% Then M_20# = MClear%
575     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
576     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
577     If M_20# = MContinue% Then GoTo *RE_POSITIONING
578     M_Out(12262) = 1 Dly 0.5
579     *CompPositioning
580     '
581 '    Wait M_In(11888) = 1        '�˂����{2��~1��M
582     MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
583     *RE_ERR_REL_2
584     If M_20# = MContinue% Then M_20# = MRtn2
585     If MRtn = 0 Then
586         MRtn2 = 1       'MRtn2������
587         M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
588         Mov PInitialPosition  '"�C�j�V�����ɖ߂铮��"
589         MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
590         If MRtn2 = 0 Then
591             MRtn2 = M_20#                   '�ʒu���ߖߒ[�G���[�Ȃ�M_20#�������ꎞ���
592             M_20# = MClear%                 'M_20#������
593             fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
594             If M_20# = MNext% Then M_20# = MRtn2        'M_20#�ɔ����l����
595                 '�ʒu���߃G���[���������čH���𔲂���ꍇ��~�������s��
596             If M_20# = MNgProcess% Then M_20# = MAbout%
597             Break
598         EndIf
599         Break
600             EndIf
601     If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
602     If MRtn = 0 Then GoTo *ASSY_ERROR_END
603     '
604 '
605 '    Mov PProductOnPltGet_1      '�{�̎󂯎����(�����ʒu�ύX2/11����)
606     '
607     *RE_PLT_GET_1
608     '
609     M_Out(12256) = 0            '�{�̃`���b�N��OFF(�����ʒu�ύX2/11����)
610     M_Out(12257) = 1            '�{�̃`���b�N�JON
611     '
612 '    Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
613     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
614     If MRtn = 1 Then GoTo *CompPltGet1
615     fErrorProcess(11,244,284,0)
616     If M_20# = MNext% Then M_20# = MClear%
617     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
618     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
619     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
620     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
621     *CompPltGet1
622     Mov PProductOnPltGet_1      '�{�̎󂯎����(�����ʒu�ύX2/11����)
623     '
624     Ovrd 25
625 '    Fine 0.05 , P
626     Mvs PProductOnPltGet        '�{�̎󂯎��ʒu
627     Dly 0.1
628     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
629     M_Out(12256) = 1            '�{�̃`���b�N��ON
630 '    Fine 0 , P
631     '
632     M_Out(12263) = 1 Dly 0.5                    '�{�̈ʒu���ߖߒ[ON
633 '    Wait M_In(11274) = 1     '�{�̈ʒu���ߖߒ[���o
634     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '�{�̈ʒu���ߖߒ[���o
635     If MRtn = 1 Then GoTo *CompPltGet2
636     M_Out(12256) = 0                            '�{�̃`���b�N��OFF
637     M_Out(12257) = 1                            '�{�̃`���b�N�JON
638     Dly 2.0
639     Mvs PProductOnPltGet_1
640     Mov PProductOnPltGet_2
641     fErrorProcess(11,234,284,0)
642     If M_20# = MNext% Then M_20# = MClear%
643     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
644     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
645     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
646     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
647     Mov PProductOnPltGet_1
648     Mvs PProductOnPltGet
649     M_Out(12257) = 0                            '�{�̃`���b�N�JOFF
650     M_Out(12256) = 1                            '�{�̃`���b�N��ON
651     Dly 2.0
652     *CompPltGet2
653     '
654 '    Wait M_In(11264) = 1        '�{�̌��o
655     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '�{�̌��o
656     If MRtn = 1 Then GoTo *CompPltGet3
657     M_Out(12256) = 0            '�{�̃`���b�N��OFF
658     M_Out(12257) = 1            '�{�̃`���b�N�JON
659     Dly 2.0
660     Mvs PProductOnPltGet_1
661     Mov PProductOnPltGet_2
662     fErrorProcess(11,252,284,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
665     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
666     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
667     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
668     Mov PProductOnPltGet_1
669     Mvs PProductOnPltGet
670     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
671     M_Out(12256) = 1            '�{�̃`���b�N��ON
672     Dly 2.0
673     *CompPltGet3
674     '
675 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
676     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
677     If MRtn = 1 Then GoTo *CompPltGet4
678     M_Out(12256) = 0            '�{�̃`���b�N��OFF
679     M_Out(12257) = 1            '�{�̃`���b�N�JON
680     Dly 2.0
681     Mvs PProductOnPltGet_1
682     Mov PProductOnPltGet_2
683     Dly 0.1
684     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
685     M_Out(12256) = 1            '�{�̃`���b�N��ON
686     Dly 3.0
687     fErrorProcess(11,245,284,0)
688     If M_20# = MNext% Then M_20# = MClear%
689     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
690     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
691     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
692     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
693     M_Out(12256) = 0            '�{�̃`���b�N��OFF
694     M_Out(12257) = 1            '�{�̃`���b�N�JON
695     Dly 2.0
696     Mov PProductOnPltGet_1
697     Mvs PProductOnPltGet
698     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
699     M_Out(12256) = 1            '�{�̃`���b�N��ON
700     Dly 2.0
701     *CompPltGet4
702     '
703     Dly 0.5                     '�O�̂��߃f�B���C
704     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
705     Mvs PProductOnPltGet_1      '�{�̎󂯎����
706     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
707     Ovrd 100
708     Mov PProductOnPltGet_2      '�{�̎󂯎������_
709     '
710     '���i���˂����{2�ɒu��
711     Ovrd 50                     '�������q�[�g�V���N�O��Ή�100��50(5/12����)
712     Mov PProductOnRoboSet_3     '�o�H
713     '
714     *RE_ROBO_SET_1
715     '
716     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
717     M_Out(12258) = 1            'DVD���J�`���b�N��ON
718 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
719     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
720     If MRtn = 1 Then GoTo *CompRoboSet1
721     fErrorProcess(11,269,284,0)
722     If M_20# = MNext% Then M_20# = MClear%
723     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
724     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
725     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
726     *CompRoboSet1
727 '
728     Mov PProductOnRoboSet_2     '�˂����{���i�u�������_
729 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
730     Ovrd 10                     '�I�[�o�[���C�h�ύX(22/12/09����)
731     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
732     Dly 0.1                     '�O�̂���Dly(22/12/09����)
733     Mvs PProductOnRoboSet       '�˂����{���i�u���ʒu
734     M_Out(12866) = 1 Dly 0.3    '�˂����{2����ĊJ(��~1�`��~2)
735 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
736     MScrewRoboNgFlg% = 0
737     MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
738     If MRtn = 0 Then
739         MScrewRoboNgFlg% = 1
740     EndIf
741 '
742     *RE_ROBO_SET_2
743 '
744     M_Out(12256) = 0            '�{�̃`���b�N��OFF
745     M_Out(12257) = 1            '�{�̃`���b�N�JON
746 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
747     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
748     If MRtn = 1 Then GoTo *CompRoboSet2
749     fErrorProcess(11,244,284,0)
750     If M_20# = MNext% Then M_20# = MClear%
751     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
752     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
753     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
754     *CompRoboSet2
755     '
756     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
757     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
758 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
759     Ovrd 100
760     Mov PProductOnRoboSet_3     '�o�H
761     '
762     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
763     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
764 '
765 '
766 '
767     '
768     '�w�ʔ����(�R���v���C�A���X���[�h����11/8����)
769     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
770     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~2�`��~3)
771 '    MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20��M�ʒu�̕ύX(����)
772 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
773     Mov PPlateBackGet_1         '�w�ʔ󂯎����
774     '
775     *RE_PLATE_GET
776     '
777     Fine 0.05 , P
778     Ovrd 25
779     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
780     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
781     M_Out(12256) = 1            '�{�̃`���b�N��ON
782     '
783 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
784     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
785     If MRtn = 1 Then GoTo *CompPlateGet_1
786     M_Out(12256) = 0            '�{�̃`���b�N��OFF
787     M_Out(12257) = 1            '�{�̃`���b�N�JON
788     Mvs PPlateBackGet_1
789     fErrorProcess(11,245,293,0) '284��293�ɕύX6/7����
790     If M_20# = MNext% Then M_20# = MClear%
791     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
792     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
793     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
794     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
795     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
796     M_Out(12256) = 1            '�{�̃`���b�N��ON
797     *CompPlateGet_1
798     Fine 0 , P
799     '
800     Ovrd 10
801     Accel 10 , 10               '25,100��10,10(22/12/09����)
802     Dly 0.7                     '�f�B���C���Ԓ��ߒ�(�c���͊m��)
803 '    CmpG 0.7,0.7,,,,,,       'X,Y���Q�C����0.7�ɕύX
804 '    ColChk Off                  '�Փˌ��mOFF
805 '    Cmp Pos , &B11          'X,Y���R���v���C�A���X���[�h�J�n
806     Cnt 1 , 10 , 10
807     Mvs PPlateBackGet_1         '�w�ʔ󂯎����
808 '    Cmp Off                     '�R���v���C�A���X���[�h�I��
809 '    ColChk On                   '�Փˌ��mON
810     Ovrd 25                     '�I�[�o�[���C�h�ύX50��25(22/12/09����)
811     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
812     Ovrd 100
813     Accel 100 , 100
814     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '�w�ʃp�l���`�F�b�N
815     If MRtn = 1 Then GoTo *CompPlateGet_2
816     Cnt 0
817     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/7����
818     If M_20# = MNext% Then M_20# = MClear%
819     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
820     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
821     If M_20# = MContinue% Then
822         Mov PPlateBackGet_1
823         Dly 0.3
824         M_Out(12256) = 0            '�{�̃`���b�N��OFF
825         M_Out(12257) = 1            '�{�̃`���b�N�JON
826         Dly 2.0
827     EndIf
828     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
829     *CompPlateGet_2    '
830     '�w�ʔ�u��
831 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
832     ColChk Off
833     Mov PPlateBackSet_13        '�w�ʔu�����
834     Cnt 0
835     '
836     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '������x�w�ʃp�l���`�F�b�N
837     If MRtn = 1 Then GoTo *CompPlateGet_3
838     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/7����
839     If M_20# = MNext% Then M_20# = MClear%
840     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
841     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
842     If M_20# = MContinue% Then
843         Mov PPlateBackGet_2
844         Mov PPlateBackGet_1
845         M_Out(12256) = 0            '�{�̃`���b�N��OFF
846         M_Out(12257) = 1            '�{�̃`���b�N�JON
847         Dly 2.0
848     EndIf
849     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
850     *CompPlateGet_3
851     '
852     ' ���i�����v�����M
853     M_Out(12787) = 1
854 '
855     MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
856     If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
857     If MRtn = 0 Then GoTo *ASSY_ERROR_END
858 '
859     Mov PPlateBackSet_12        '�ܓ���O���_
860     Cnt 0
861     Ovrd 25
862     Accel 25 , 25
863     Mvs PPlateBackSet_11        '�ܓ��ꍞ�ݑO
864     Mvs PPlateBackSet_10        '�ܓ��ꍞ��1
865     Mvs PPlateBackSet_9         '�ܓ��ꍞ��2
866 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
867 '    Cmp Pos, &B001000
868     Cnt 1           '0.1��0.2mm�ߖT�ɕύX(221219����)
869     Mov PPlateBackSet_8         '�o�H1
870     Mov PPlateBackSet_7         '�o�H2
871     Mov PPlateBackSet_6         '�o�H3
872     Mov PPlateBackSet_5         '�o�H4
873     Mov PPlateBackSet_4         '�o�H5
874     Mov PPlateBackSet_3         '�o�H6-
875     Mov PPlateBackSet_2         '�o�H7
876     Mov PPlateBackSet_1         '�o�H8
877     Mov PPlateBackSet           '�w�ʔ����ʒu
878 '    Cmp Off
879     Accel 100 , 100
880     Cnt 0
881     Dly 0.1
882     *RE_PLATE_SET
883     M_Out(12256) = 0            '�{�̃`���b�N��OFF
884     M_Out(12257) = 1            '�{�̃`���b�N�JON
885     '
886 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
887     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
888     If MRtn = 1 Then GoTo *CompPlateSet
889     fErrorProcess(11,244,284,0)
890     If M_20# = MNext% Then M_20# = MClear%
891     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
892     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
893     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
894     *CompPlateSet
895     '
896 '-----�b�艟��-------------------------------------(22/12/14����)��������
897 *RE_BUCK_PUSH
898     M_20# = MClear%
899     Mov PPlateBackPush_2
900 '
901     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
902     M_Out(12256) = 1            '�{�̃`���b�N��ON
903 '
904     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
905 '
906     If MRtn = 1 Then GoTo *CompBuckPushSetting  '����Ȃ牟�������
907 '
908     fErrorProcess(11,245,287,0) '�[�Z���T�[NG���G���[�\��
909         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
910         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
911         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NG�������ꂽ��G���[�G���h��
912         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       '���g���C�������ꂽ�������x����
913 '
914 *CompBuckPushSetting
915 '
916     Mvs PPlateBackPush_1
917     Ovrd 10
918     Mvs PPlateBackPush
919 '    Dly 0.1     '�N�����v������̂ō폜(221219����)
920 '�w�ʃN�����v��������(12/15)
921     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
922     MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
923         If MRtn = 0 Then
924             Mvs PPlateBackPush_1
925             Mov PPlateBackSet_13
926             Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
927         EndIf
928         If MRtn = 0 Then GoTo *ASSY_ERROR_END
929 '�w�ʃN�����v�����܂�(12/15)
930     Ovrd 50                     '20��50�ɕύX(221219����)
931     Mvs PPlateBackPush_1
932 *RE_CHUCK_OPEN
933     M_20# = MClear%
934     M_Out(12256) = 0            '�{�̃`���b�N��OFF
935     M_Out(12257) = 1            '�{�̃`���b�N�JON
936     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
937     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
938     fErrorProcess(11,244,284,0)
939         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
940         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
941         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NG�������ꂽ��G���[�G���h��
942         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       '���g���C�������ꂽ�������x����
943 *CompChuckOpenForBackPush
944 '-----�b�艟��-------------------------------------(22/12/14����)�����܂�
945 '
946     ColChk On
947     Mov PPlateBackSet_13        '�w�ʔu�����
948 '    M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
949     Ovrd 100
950     '�˂����{���i�N�����v�Œ�҂�(�R�����g�A�E�g221215����)
951 '    Wait M_In(11891) = 1        '�˂����{2��~4��M
952 'MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
953 'If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
954 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
955     '
956     '�˂����{�������ݑ҂�
957 '    M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)   '
958     '�u���ʒu�摜����
959 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
960 '    Mov PPlateBackCheck_2       '�ʉߓ_
961     If M_In(11369) = 0 Then GoTo *CompCheck
962     Mvs PPlateBackCheck         '�m�F�ʒu
963     '
964 *RE_CHECK
965     PInspPosition(1) = PPlateBackCheck
966     MInspGroup%(1) = 2
967     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
968     If M_In(11374) = 0 Then MRtn = 1
969     If MRtn = 1 Then GoTo *CompCheck
970     fErrorProcess(11,43,23,0)
971     If M_20# = MNext% Then M_20# = MClear%
972     If M_20# = MAbout% Or M_20# = MNgProcess% Then
973         Mov PPlateBackSet_12
974         Mov PInitialPosition
975     EndIf
976     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
977     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
978     If M_20# = MContinue% Then GoTo *RE_CHECK
979 *CompCheck
980 '
981     Mov PPlateBackSet_13        '�w�ʔu�����
982 ''    ' ���i�����v�����M
983 '    M_Out(12787) = 1
984 '    Mov PPlateBackCheck_2       '�ʉߓ_
985 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
986     '
987     '�˂����{�������ݑ҂�
988     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)
989 '
990     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
991     M_Out(12259) = 1            'DVD���J�`���b�N�JON
992     '
993     'DVD���J�����
994     Mvs PMechaGet_3             '�o�H1
995     Mvs PMechaGet_2             'DVD���J�󂯎������_
996 '    Wait M_In(11272)            '���i�����@Ready
997 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '���i�����@Ready
998 '    If MRtn = 0 Then
999 '        fErrorProcess()         '�G���[����
1000 '    EndIf
1001 '
1002   ' ���i�����v�����M
1003     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
1004 '    M_Out(12787) = 1
1005     '    ' ���i���������҂�(�����ύX2/27����)
1006 *RE_FEEDER_READY
1007 '    Wait M_In(11810) = 1
1008 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
1009 If MRtn = 1 Then GoTo *CompFeederReady
1010 '   ' ���i�����v���I��
1011 M_Out(12787) = 0
1012 fErrorProcess(11,289,290,0)                '284��290�ɕύX6/7����
1013 If M_20# = MNext% Then M_20# = MClear%
1014 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1015     Mov PMechaGet_2
1016     Mov PMechaGet_3
1017     Mov PMechaGet_4
1018     Mov PInitialPosition
1019 EndIf
1020 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1021 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1022     ' ���i�����v��
1023 M_Out(12787) = 1
1024 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1025 *CompFeederReady
1026 '    ' ���i�����v���I��
1027     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1028     M_Out(12787) = 0
1029 '
1030     Mvs PMechaGet_1             'DVD���J�󂯎����
1031     '
1032     *RE_MECHA_GET_1
1033     '
1034     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1035     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1036     '
1037 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1038     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1039     If MRtn = 1 Then GoTo *CompMechaGet1
1040     Mvs PMechaGet_2
1041     Mvs PMechaGet_3
1042     Mov PMechaGet_4
1043     fErrorProcess(11,270,284,0)
1044     If M_20# = MNext% Then M_20# = MClear%
1045     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1046     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1047     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1048     Mov PMechaGet_3
1049     Mvs PMechaGet_2
1050     Mvs PMechaGet_1
1051     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1052     *CompMechaGet1
1053     '
1054     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1055     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1056 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1057     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1058     If MRtn = 1 Then GoTo *CompMechaGet2
1059     Mvs PMechaGet_2
1060     Mvs PMechaGet_3
1061     Mov PMechaGet_4
1062     fErrorProcess(11,271,284,0)
1063     If M_20# = MNext% Then M_20# = MClear%
1064     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1065     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1066     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1067     Mov PMechaGet_3
1068     Mvs PMechaGet_2
1069     Mvs PMechaGet_1
1070     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1071     *CompMechaGet2
1072     '
1073     Ovrd 25
1074     Mvs PMechaGet               'DVD���J�󂯎��ʒu
1075     Dly 0.1                     '�ʒu�o���@(22/12/09����)
1076 '
1077     MRtn = 0
1078     MRtn2 = 0
1079     *RE_MECHA_GET_2
1080     If M_20# = MContinue% Then M_20# = MClear%
1081     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1082     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1083     '
1084 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1085     If MRtn = 1 Then Dly 1.0
1086     If MRtn = 1 Then GoTo *CompMechaGet3
1087     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1088     If M_20# = MNext% Then GoTo *CompMechaGet3
1089     If MRtn = 1 Then GoTo *CompMechaGet3
1090     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1091     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1092     Dly 2.0
1093     Mvs PMechaGet_1
1094     Mvs PMechaGet_2
1095     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1096     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1097     Mvs PMechaGet_3
1098     Mov PMechaGet_4
1099     fErrorProcess(11,269,284,0)
1100     If M_20# = MNext% Then
1101         M_20# = MClear%
1102         MRtn = 1
1103     EndIf
1104     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1105     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1106     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1107     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1108     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1109     Mov PMechaGet_3
1110     Mvs PMechaGet_2
1111     Mvs PMechaGet_1
1112     Mvs PMechaGet
1113     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1114     *CompMechaGet3
1115     M_20# = MClear%
1116     '
1117 '    Wait M_In(11267) = 1        'DVD���J���o
1118     If MRtn2 = 1 Then GoTo *CompMechaGet4
1119     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVD���J���o
1120     If MRtn2 = 1 Then GoTo *CompMechaGet4
1121     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1122     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1123     Dly 2.0
1124     Mvs PMechaGet_1
1125     Mvs PMechaGet_2
1126     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1127     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1128     Mvs PMechaGet_3
1129     Mov PMechaGet_4
1130     fErrorProcess(11,273,284,0)
1131     If M_20# = MNext% Then
1132         M_20# = MClear%
1133         MRtn2 = 1
1134     EndIf
1135     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1136     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1137     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1138     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1139     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1140     Mov PMechaGet_3
1141     Mvs PMechaGet_2
1142     Mvs PMechaGet_1
1143     Mvs PMechaGet
1144     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1145     *CompMechaGet4
1146     M_20# = MClear%
1147     Dly 0.5
1148     '
1149     Mvs PMechaGet_1             'DVD���J�󂯎����
1150 '    *RE_MECHA_GET_3
1151     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1152     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1153 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1154     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1155 '    If MRtn = 1 Then GoTo *CompMechaGet5       '�����ʒu�ύX2/11����
1156 '    fErrorProcess(11,272,284,0)
1157 '    If M_20# = MNext% Then M_20# = MClear%
1158 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1159 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1160 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1161 '    *CompMechaGet5
1162     '
1163     If MRtn = 1 Then Ovrd 50    '�O�̂��߃I�[�o�[���C�h�ύX100��50(22/12/09����)
1164     Mvs PMechaGet_2             'DVD���J�󂯎������_
1165 '    ' ���i�����v���I��
1166     M_Out(12787) = 0
1167 '    ' ���i�擾�������M(�p���X)
1168     M_Out(12800) = 1 Dly 0.5
1169     Mvs PMechaGet_3             '�o�H1
1170     Mov PMechaGet_4             '�o�H2
1171 '
1172     *RE_MECHA_GET_3
1173     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1174     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1175     If MRtn = 1 Then GoTo *CompMechaGet5
1176     fErrorProcess(11,272,284,0)
1177     If M_20# = MNext% Then M_20# = MClear%
1178     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1179     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1180     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1181     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1182     *CompMechaGet5
1183     '
1184     'DVD���J�����u����֒u��
1185 '    Wait M_In(11920) = 0             'BaseUnit6�����u����t���O�m�F(�����ʒu�ύX2/11����)
1186 '
1187 '   ���u���䂪��]�����m�F
1188     *Loop_CW_CCW_1
1189     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1190     *Next_CW_CCW_1
1191     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1           'BaseUnit6�����u����t���O�m�F
1192     *OK_FLG_1
1193 '
1194     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1195     '
1196     MRtn = 1
1197     'DVD���J�����u����ɒu����Ă��Ȃ����̊m�F(�ǉ���������10/1����)
1198     If M_In(11931) = 1 Then          '��]�����̊m�F(CW����)
1199         If M_In(11928) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1200             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1201             Wait M_In(11930) = 1     '���u�����]�҂�
1202             'If M_In(11929) = 0 Then  '��]��ɂ܂�BaseUnit5���ɒu����Ă���ꍇ
1203                 '�G���[����(BaseUnit6���ɂĐ���ȓ��삪����Ă��Ȃ�)
1204             'Endif
1205         EndIf
1206     ElseIf M_In(11930) = 1 Then      '��]�����̊m�F(CCW����)
1207         If M_In(11929) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1208             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1209             Wait M_In(11931) = 1     '���u�����]�҂�
1210             MRtn = 0
1211         EndIf
1212     Else
1213         MRtn = 0'�G���[����(���u���䂪����ȓ�������Ă��Ȃ�)
1214     EndIf
1215     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1216 '
1217 *Loop_CW_CCW_S
1218     fnAutoScreenComment(530)    '��ԕ\��[�H���U�̓���I���҂�] 2022/04/26 �n��
1219 'Ver 0.4 �ǉ� -----------------------
1220 '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1221     MRtn = 0
1222     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1223     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1224     If MRtn = 1 Then Dly 0.7
1225     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1226     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1227 'Ver 0.4 �����܂� -------------------
1228 '
1229     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1230     Mov PMechaSet_3             'DVD���J���u�����_1(�����ʒu�ύX2/27����)
1231 '
1232 *Loop_CW_CCW_2
1233 'Ver 0.4 �ǉ� -----------------------
1234     '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1235     MRtn = 0
1236     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1237     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1238     If MRtn = 1 Then Dly 0.7
1239     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1240     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1241 'Ver 0.4 �����܂� -------------------
1242 '
1243     Mov PMechaSet_2             'DVD���J���u�����_2(�����ʒu�ύX2/27����)
1244 '
1245 '    *Loop_CW_CCW_2  '���u����̏�Ԃ�������x�m�F����(�R�����g�A�E�g2/27����)
1246 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1247 '    *Next_CW_CCW_2
1248 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2           'BaseUnit6�����u����t���O�m�F
1249 '    *OK_FLG_2
1250 '
1251 '    M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1252 '
1253     '
1254     *RE_MECHA_SET_1
1255     If M_20# = MContinue% Then M_20# = MClear%
1256     Ovrd 25
1257     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1258     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1259 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1260     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1261     If MRtn = 1 Then GoTo *CompMechaSet1
1262     Mov PMechaSet_3
1263     Mov PMechaGet_4
1264     fErrorProcess(11,271,284,0)
1265     If M_20# = MNext% Then M_20# = MClear%
1266     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1267     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1268     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1269     Mov PMechaSet_3
1270     Mov PMechaSet_2
1271     Ovrd 100
1272     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1273     *CompMechaSet1
1274     '
1275     *RE_MECHA_SET_12
1276     Fine 0.05 , P
1277 '    Wait M_In(11920) = 0        'BaseUnit6�����u����t���O�m�F
1278 '    M_Out(12912) = 1            '���u����t���O����
1279     If M_In(11931) = 1 Then     '��]�����̊m�F(CW����)(�ǉ������܂�10/1����)
1280         Mov PMechaSet1_1        'DVD���J���u�����1
1281         Ovrd 10
1282         Mvs PMechaSet1          'DVD���J���u���ʒu1
1283         Dly 0.1
1284         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1285         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1286 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1287         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1288         Mvs PMechaSet1_1        'DVD���J���u�����1
1289     ElseIf M_In(11930) = 1 Then '��]�����̊m�F(CCW����)(�ǉ���������10/1����)
1290         Mov PMechaSet2_1        'DVD���J���u�����
1291         Ovrd 10
1292         Mvs PMechaSet2          'DVD���J���u���ʒu2
1293         Dly 0.1
1294         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1295         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1296 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1297         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1298         Mvs PMechaSet2_1        'DVD���J���u�����2
1299     'Else
1300         '�G���[��(���u���䂪����Ȉʒu�ɖ���)
1301     EndIf                       '�ǉ������܂�10/1����
1302     Fine 0 , P
1303     '
1304     If MRtn = 1 Then GoTo *CompMechaSet2
1305     fErrorProcess(11,270,284,0)
1306     If M_20# = MNext% Then M_20# = MClear%
1307     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1308     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1309     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1310     *CompMechaSet2
1311     '
1312     Ovrd 100
1313     *RE_MECHA_SET_2
1314     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1315     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1316 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1317     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1318     If MRtn = 1 Then GoTo *CompMechaSet3
1319     fErrorProcess(11,272,284,0)
1320     If M_20# = MNext% Then M_20# = MClear%
1321     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1322     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1323     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1324     *CompMechaSet3
1325     '
1326     Mov PMechaSet_2             'DVD���J���u�����_2
1327     M_Out(12912) = 0                  '���u����t���O���(�ǉ�10/1����)
1328     '
1329     '�˂����{2�̐��i�����
1330     Mov PProductOnRoboGet_4     '�o�H3����4��
1331     M_Out(12259) = 0            'DVD���J�`���b�N�JOFF
1332     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1333 '    Wait M_In(11876) = 1        '�˂����{2�����ҋ@����M
1334 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1335 If MRtn = 0 Then Mov PInitialPosition   '"�C�j�V�����ɖ߂铮��"
1336 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1337 '
1338     *RE_ROBO_GET_1
1339 '
1340     M_Out(12259) = 0            'DVD���J�`���b�N�JOFF
1341     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1342     If M_20# = MContinue% Then Dly 0.5
1343 '
1344 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1345     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1346     If MRtn = 1 Then GoTo *CompRoboGet1
1347     fErrorProcess(11,269,284,0)
1348     If M_20# = MNext% Then M_20# = MClear%
1349     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1350     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1351     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1352     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1353     *CompRoboGet1
1354     '
1355     Ovrd 50
1356     Mov PProductOnRoboGet_3     '�˂����{���i�������_2����3��
1357     Ovrd 20
1358     Mvs PProductOnRoboGet_2     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����2��
1359     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1360     Ovrd 10
1361     Mvs PProductOnRoboGet       '�˂����{���i���ʒu
1362     Dly 0.3
1363 '
1364     *RE_ROBO_GET_2
1365 '
1366     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1367     M_Out(12256) = 1            '�{�̃`���b�N��ON
1368 '
1369 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
1370     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
1371     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1372     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1373     M_Out(12257) = 1            '�{�̃`���b�N�JON
1374     Dly 2.0
1375     Mvs PProductOnRoboGet_1
1376     Mvs PProductOnRoboGet_2
1377     Mov PProductOnRoboGet_3
1378     Mov PProductOnRoboGet_4
1379     Mov PInitialPosition
1380     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1381     M_Out(12256) = 1            '�{�̃`���b�N��ON
1382     Dly 1.0
1383     fErrorProcess(11,245,284,0)
1384     If M_20# = MNext% Then MRtn = 1
1385     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1386     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1387     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1388     M_Out(12257) = 1            '�{�̃`���b�N�JON
1389     Dly 2.0
1390     Mov PProductOnRoboGet_4
1391     Mov PProductOnRoboGet_3
1392     Mov PProductOnRoboGet_2
1393     Mvs PProductOnRoboGet_1
1394     Mvs PProductOnRoboGet
1395     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1396     *CompRoboGet2
1397     M_20# = MClear%
1398     '
1399     Accel 30 , 100
1400     Dly 0.3
1401     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1402     Ovrd 25
1403     Mvs PProductOnRoboGet_2     '�˂����{���i�������_(9/27�b��R�����g�A�E�g)12/15�R�����g����
1404     Mvs PProductOnRoboGet_3     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����3��
1405     Ovrd 50                     '�I�[�o�[���C�h�ύX(22/12/09����)
1406     Mov PProductOnRoboGet_4     '�o�H3����4��
1407     Accel 50 , 50               '�����x�ύX(22/12/09����)
1408 '
1409     M_Out(12868) = 1 Dly 0.5    '�˂����{2���슮���𑗐M
1410 '    *RE_ROBO_GET_3                                     '�����ʒu�ύX2/11����
1411 '    M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1412 '    M_Out(12259) = 1            'DVD���J�`���b�N�JON
1413 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1414 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1415 '    If MRtn = 1 Then GoTo *CompRoboGet3
1416 '    fErrorProcess(11,270,284,0)
1417 '    If M_20# = MNext% Then M_20# = MClear%
1418 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1419 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1420 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1421 '    *CompRoboGet3
1422     '
1423     '�p���b�g�֐��i��u��
1424     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1425     Accel 100 , 100             '�����x�ύX(22/12/09����)
1426     Mov PProductOnPltSet_1      '�{�̒u���ʒu���
1427     Ovrd 10
1428     Mvs PProductOnPltSet        '�{�̒u���ʒu
1429     Dly 0.3
1430 '
1431     *RE_PLT_SET
1432 '
1433     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1434     M_Out(12257) = 1            '�{�̃`���b�N�JON
1435 '
1436 '    Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
1437     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1438     If MRtn = 1 Then GoTo *CompPltSet
1439     fErrorProcess(11,244,284,0)
1440     If M_20# = MNext% Then M_20# = MClear%
1441     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1442         Mvs PProductOnPltSet_1
1443         Mov PProductOnPltSet_2
1444         Mov PInitialPosition
1445     EndIf
1446     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1447     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1448     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1449     *CompPltSet
1450 '
1451     Mvs PProductOnPltSet_1      '�{�̒u���ʒu���
1452     Ovrd 100
1453     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1454 '    Mov PInitialPosition        '�C�j�V�����|�W�V����
1455     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
1456     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
1457     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1458     *RE_FIN_INI
1459     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1460     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1461 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1462     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1463     If MRtn = 1 Then GoTo *CompFinIni
1464     fErrorProcess(11,270,284,0)         '���g���C��NG���Ƀ��g���C��������
1465     If M_20# = MNext% Then M_20# = MClear%
1466     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1467     If M_20# = MNgProcess% Then GoTo *RE_FIN_INI
1468     If M_20# = MContinue% Then GoTo *RE_FIN_INI
1469     *CompFinIni
1470     '
1471     '�`�P�b�gID��������
1472     M_20# = MAssyOK%
1473     *ASSY_ERROR_END
1474     *AssyEnd
1475     *fnAssyStart_FEndPosi
1476 FEnd
1477 '
1478 '��fnPiasCheck
1479 ''' <summary>
1480 ''' PIAS�`�P�b�g�Ǎ���
1481 ''' </summary>
1482 ''' <returns>   0 : NG
1483 '''             1 : OK(�Ǎ��݊���)
1484 ''' </returns>
1485 ''' <remarks>
1486 ''' Date   : 2021/07/07 : M.Hayakawa
1487 ''' </remarks>'
1488 Function M% fnPiasCheck
1489     fnPiasCheck = 0
1490     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1491     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1492 '
1493 *RETRY_PIAS
1494     M_20# = MClear%
1495     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1496     '
1497     '�yID�`�P�b�g�ǂݍ��݁z
1498     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1499     MInspGroup%(1) = 1              '����G�ԍ�
1500     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1501 '
1502     '�G���[�̏ꍇ
1503     If MRtn <> 1 Then
1504         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1505         If MRtn <> 1 Then
1506             'D720 -> D1300 �R�s�[�v��
1507             M_Out(12565) = 1
1508             Dly 0.5
1509             M_Out(12565) = 0
1510             '�G���[�����L�q
1511             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1512             'GOT KEY���͑҂�
1513             MKeyNumber = fnKEY_WAIT()
1514             '
1515             Select MKeyNumber
1516                 Case MNext%         '���ւ�I�������ꍇ
1517                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1518                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1519                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1520                     Break
1521                 Case MAbout%        '��~��I�������ꍇ
1522                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1523                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1524                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1525                     Break
1526                 Case MNgProcess%    'NG��I�������ꍇ
1527                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1528                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1529                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1530                     Break
1531                 Case MContinue%     '�p����I�������ꍇ
1532                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1533                     M_20# = MContinue%
1534                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1535                     Break
1536             End Select
1537         EndIf
1538     EndIf
1539 '----------D720 -> D1300 �R�s�[�v��----------
1540     M_Out(12565) = 1
1541     Dly 0.5
1542     M_Out(12565) = 0
1543 '----------�ʐM�m�F������----------
1544     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1545     MRtn = 0                ' ������
1546     M_20# = MClear%         ' ������
1547     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1548     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1549     If MRtn <> 1 Then
1550         If M_20# = MContinue% Then
1551             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1552         Else
1553             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1554         EndIf
1555     EndIf
1556 '----------�H�������m�F----------
1557     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1558     MRtn = 0                ' ������
1559     M_20# = MClear%         ' ������
1560     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1561     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1562     If MRtn <> 1 Then
1563         If M_20# = MContinue% Then
1564             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1565         Else
1566             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1567         EndIf
1568     EndIf
1569     '
1570     fnPiasCheck = 1
1571     *fnPiasCheck_End
1572 FEnd
1573 '
1574 '��fnPCComuCheck
1575 ''' <summary>
1576 ''' PC-PLC�ʐM�`�F�b�N
1577 ''' </summary>
1578 ''' <returns>   0 : NG
1579 '''             1 : OK(�Ǎ��݊���)
1580 ''' </returns>
1581 ''' <remarks>
1582 ''' Date   : 2021/07/07 : M.Hayakawa
1583 ''' </remarks>'
1584 Function M% fnPCComuCheck
1585     fnPCComuCheck = 0
1586     MJudge% = 0                                  '������
1587     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1588     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1589     '
1590     For MStaNo = 0 To 5
1591         '
1592         If M_In(MIN_PIAS_ComOK%) = 1 Then
1593             'PC�ʐMOK(M400)
1594             MJudge% = MOK%
1595             MStaNo = 5
1596             Break
1597         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1598             'toRBT_�ʐM�m�Ftime out
1599             MJudge% = MNG%
1600             MCommentD1001 = 15
1601             MCommentD1002 = 21
1602             MStaNo = 5
1603             Break
1604         Else
1605             'toRBT_�ʐM�m�Ftime out
1606             MJudge% = MNG%
1607             MCommentD1001 = 14
1608             MCommentD1002 = 21
1609             Break
1610         EndIf
1611     Next MStaNo
1612     '
1613     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1614     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1615     '
1616     '�G���[���
1617     If MJudge% <> MOK% Then
1618         M_20# = MClear%     '������
1619         '�G���[�����L�q
1620         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1621         'GOT KEY���͑҂�
1622         MKeyNumber = fnKEY_WAIT()
1623         '
1624         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1625             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1626             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1627             Break
1628         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1629             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1630             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1631             Break
1632         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1633             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1634             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1635             Break
1636         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1637             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1638             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1639             Break
1640         EndIf
1641     Else
1642         'OK�̏ꍇ
1643         fnPCComuCheck = 1
1644     EndIf
1645 FEnd
1646 '
1647 '��fnProcessCheck
1648 ''' <summary>
1649 ''' �H�������m�F
1650 ''' </summary>
1651 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1652 '''             -1�F�O�H������NG  -2�F���H����������
1653 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1654 '''             -5�F���������G���[
1655 ''' </returns>
1656 ''' <remarks>
1657 ''' Date   : 2021/07/07 : M.Hayakawa
1658 ''' </remarks>'
1659 Function M% fnProcessCheck
1660     fnProcessCheck = 0
1661     MJudge% = MNG%      '��UNG���������Ƃ���
1662 '----------�H�������m�F----------
1663     MCommentD1001 = 0   '�R�����g������
1664     For MStaNo = 0 To 5
1665         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1666         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1667         '
1668         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1669             MJudge% = MOK%
1670             fnAutoScreenComment(85)     ' AUTO���
1671             MStaNo = 5
1672             Break
1673         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1674             MFlgLoop% = 0
1675             MJudge% = MNG%
1676             MCommentD1001 = 27
1677             MCommentD1002 = 22
1678             fnAutoScreenComment(94)     ' AUTO���
1679             fnProcessCheck = -2         ' NG��-2��Ԃ�
1680             MStaNo = 5
1681             Break
1682         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1683            MJudge% = MNG%
1684             MCommentD1001 = 31
1685             MCommentD1002 = 22
1686             fnAutoScreenComment(83)     ' AUTO���
1687             fnProcessCheck = -3         ' NG��-3��Ԃ�
1688             MStaNo = 5
1689             Break
1690         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1691             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1692             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1693             MJudge% = MNG%
1694             MCommentD1001 = 32
1695             MCommentD1002 = 22
1696             fnAutoScreenComment(84)     ' AUTO���
1697             fnProcessCheck = -1         ' NG��-1��Ԃ�
1698             Dly 1.0
1699             '�H�������m�FOFF
1700             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1701             Dly 1.0
1702            'MStaNo = 5
1703             Break
1704         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1705             MFlgLoop% = 0
1706             MJudge% = MNG%
1707             MCommentD1001 = 29
1708             MCommentD1002 = 22
1709             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1710             fnProcessCheck = -5         ' NG��-5��Ԃ�
1711             MStaNo = 5
1712             Break
1713         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1714             MJudge% = MNG%
1715             If MCommentD1001 = 32 Then
1716                 '�������Ȃ�
1717             Else
1718                 MCommentD1001 = 26
1719             EndIf
1720             MCommentD1002 = 22
1721             fnProcessCheck = -4         ' NG��-4��Ԃ�
1722             MStaNo = 5
1723             Break
1724         Else
1725             MJudge% = MNG%
1726             MCommentD1001 = 28
1727             MCommentD1002 = 22
1728         EndIf
1729     Next MStaNo
1730     '�H�������m�FOFF
1731     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1732     '�ʉߗ���NG �H�������̏ꍇ
1733     If MJudge% = MPass% Then
1734         M_20# = MPass%
1735     EndIf
1736     '
1737     '�G���[���
1738     If MJudge% <> MOK% Then
1739         M_20# = MClear%     '������
1740         '�G���[�����L�q
1741         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1742         'GOT KEY���͑҂�
1743         MKeyNumber = fnKEY_WAIT()
1744         '
1745         Select MKeyNumber
1746             Case MAbout%        '��~��I�������ꍇ
1747                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1748                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1749                 Break
1750             Case MNext%         '���ւ�I�������ꍇ
1751                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1752                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1753                 Break
1754             Case MContinue%     '�p����I�������ꍇ
1755                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1756                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1757                 Break
1758             Case MNgProcess%    'NG��I�������ꍇ
1759                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1760                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1761                 Break
1762         End Select
1763     Else
1764         fnProcessCheck = 1  ' OK��1��Ԃ�
1765     EndIf
1766 FEnd
1767 '
1768 '��fnPiasWrite
1769 ''' <summary>
1770 ''' Pias �g�����ʏ����ݗv��
1771 ''' </summary>
1772 '''<param name="MFlg%">
1773 '''                 MOK%(1) = �H��������OK��������
1774 '''                 MNG%(0) = �H��������NG��������
1775 '''</param>
1776 '''<returns></returns>
1777 ''' <remarks>
1778 ''' Date   : 2021/07/07 : M.Hayakawa
1779 ''' </remarks>'
1780 Function M% fnPiasWrite(ByVal MFlg%)
1781       fnPiasWrite = 0
1782 *RETRY_PIASWRITE
1783     '
1784     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1785    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1786     If MFlg% = MOK% Then
1787         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1788     Else
1789         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1790     EndIf
1791     Dly 0.1                  '�O�̂���
1792     '
1793     'Pias�֏����݊J�n M305 -> ON
1794     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1795     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1796     '
1797     MJudge% = MNG%
1798     '
1799     For MStaNo = 0 To 5
1800         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1801             MJudge% = MOK%
1802             'MRet = fnAutoScreenComment(85)  'AUTO���
1803             MStaNo = 5
1804             Break
1805         '
1806         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1807             MJudge% = MNG%
1808             'MRet = fnAutoScreenComment(85)  'AUTO���
1809            MCommentD1001 = 34
1810            MCommentD1002 = 25
1811             MStaNo = 5
1812             Break
1813         '
1814         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1815             MJudge% = MNG%
1816             'MRet = fnAutoScreenComment(85)  'AUTO���
1817            MCommentD1001 = 35
1818            MCommentD1002 = 25
1819             MStaNo = 5
1820             Break
1821         '
1822         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1823             MJudge% = MNG%
1824             'MRet = fnAutoScreenComment(85)  'AUTO���
1825            MCommentD1001 = 36
1826            MCommentD1002 = 25
1827             MStaNo = 5
1828             Break
1829         '
1830         Else
1831             MJudge% = MNG%
1832            MCommentD1001 = 42
1833            MCommentD1002 = 25
1834         '
1835         EndIf
1836         '
1837     Next MStaNo
1838     '
1839     'Pias�֏����݊J�n M305 -> OfF
1840     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1841     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1842     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1843     '
1844     '
1845     '�ʉߗ���NG �H�������̏ꍇ
1846     If MJudge% = MPass% Then
1847         M_20# = MPass%
1848     EndIf
1849     '
1850    M_20# = MClear%     '������
1851     '
1852     '�G���[���
1853     If MJudge% < MOK% Then
1854     '
1855 '�c���Ă���������ł͎g�p���Ȃ����x��
1856 *RETRY_ERR_WRITE
1857         M_20# = MClear%     '������
1858         '�G���[�����L�q
1859         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1860         'GOT KEY���͑҂�
1861         MKeyNumber = fnKEY_WAIT()
1862         '
1863         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1864             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1865            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1866             Break
1867         '
1868         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1869             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1870             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1871         '
1872         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1873             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1874             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1875         '
1876         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1877             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1878            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1879             Break
1880         '
1881         EndIf
1882         '
1883         If M_20# = MClear% Then *RETRY_ERR_WRITE
1884         '
1885     EndIf
1886     '
1887     If M_20# = MContinue% Then *RETRY_PIASWRITE
1888     '
1889     fnPiasWrite = 1
1890     '
1891 FEnd
1892 '
1893 '��fnPCBNumberCheck
1894 ''' <summary>
1895 ''' Pias ��ԍ��ƍ��v��
1896 ''' </summary>
1897 '''<param name="%"></param>
1898 '''<param name="%"></param>
1899 '''<returns></returns>
1900 ''' <remarks>
1901 ''' Date   : 2021/07/07 : M.Hayakawa
1902 ''' </remarks>'
1903 Function M% fnPCBNumberCheck
1904       fnPCBNumberCheck = 0
1905     '
1906 *RETRY_PCBCHECK
1907     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1908     'Pias�֊�ƍ��J�n M310 -> ON
1909     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1910     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1911     '
1912     MJudge% = MNG%
1913     '
1914     For MStaNo = 0 To 5
1915         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1916             MJudge% = MOK%
1917             fnAutoScreenComment(96)  'AUTO���
1918             MStaNo = 5
1919             Break
1920         '
1921         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1922             MJudge% = MNG%
1923             fnAutoScreenComment(97)  'AUTO���
1924             MCommentD1001 = 37
1925             MCommentD1002 = 25
1926             MStaNo = 5
1927             Break
1928         '
1929         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1930             MJudge% = MNG%
1931             fnAutoScreenComment(98)  'AUTO���
1932             MCommentD1001 = 38
1933             MCommentD1002 = 25
1934             MStaNo = 5
1935             Break
1936         '
1937         ElseIf M_In(11580) = 1 Then                         'time out
1938             MJudge% = MNG%
1939             fnAutoScreenComment(99)  'AUTO���
1940             MCommentD1001 = 39
1941             MCommentD1002 = 25
1942             MStaNo = 5
1943             Break
1944         '
1945         Else
1946             MJudge% = MNG%
1947            MCommentD1001 = 41
1948            MCommentD1002 = 25
1949         '
1950         EndIf
1951         '
1952     Next MStaNo
1953     '
1954     'Pias�֊�ƍ��J�n M310 -> OfF
1955     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1956     '
1957     '
1958     '�ʉߗ���NG �H�������̏ꍇ
1959     If MJudge% = MPass% Then
1960         M_20# = MPass%
1961     EndIf
1962     '
1963    M_20# = MClear%     '������
1964     '
1965     '�G���[���
1966     If MJudge% < MOK% Then
1967     '
1968 '�c���Ă���������ł͎g�p���Ȃ����x��
1969 *RETRY_ERR_PCBNUMBER
1970         M_20# = MClear%     '������
1971         '�G���[�����L�q
1972         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1973         'GOT KEY���͑҂�
1974         MKeyNumber = fnKEY_WAIT()
1975         '
1976         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1977             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1978             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1979             Break
1980         '
1981         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1982             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1983             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1984         '
1985         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1986             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1987             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1988         '
1989         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1990             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1991             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1992             Break
1993         '
1994         EndIf
1995         '
1996         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1997         '
1998     EndIf
1999     '
2000     If M_20# = MContinue% Then *RETRY_PCBCHECK
2001 FEnd
2002 '
2003 '��ScrewTight_S2
2004 ''' <summary>
2005 ''' �˂����߂��s��
2006 ''' </summary>
2007 '''<param name="PScrewPos()">
2008 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
2009 '''             PScrewPos(2)    �F�˂����߉��_
2010 '''             PScrewPos(10)   �F�˂����ߏI������
2011 '''</param>
2012 '''<returns>����
2013 '''         0=�ُ�I���A1=����I��
2014 '''</returns>
2015 ''' <remarks>
2016 ''' Date   : 2021/07/07 : M.Hayakawa
2017 ''' </remarks>'
2018 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
2019     ScrewTight_S2 = 0
2020     MOKNGFlg = 0
2021     Ovrd 100
2022     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2023     ' �b��
2024     Ovrd 5
2025     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
2026 '    Ovrd MOvrdA
2027     '�b��}�X�N
2028 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
2029 '    Dly 0.1
2030 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
2031 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
2032 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
2033     ' �b��ړ��̂�
2034     Mvs PScrewPosition(10)
2035 '    '
2036 '    Dly 0.1
2037 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2038 '    Wait M_In(11584)=1          '����/�G���[���o
2039 '    Dly 0.1
2040 '    Spd M_NSpd
2041 '    '
2042 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
2043 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2044 '        Dly 0.1
2045 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2046 '        Dly 0.1
2047 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2048 '        Dly 0.1
2049 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
2050 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2051 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2052 '        MOKNGFlg = -1
2053 '        ScrewTight_S2 = 0
2054 '    Else
2055 '        Wait M_In(X29_Driver)=1 ' ���튮����
2056 '        Dly 0.1
2057 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2058 '        Dly 0.1
2059 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
2060 '        Dly 0.1
2061 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2062 '        Dly 0.1
2063 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2064 '        ScrewTight_S2 = 1
2065 '    EndIf
2066 ' �b��
2067     Ovrd 10
2068     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2069     Ovrd 100
2070 FEnd
2071 '
2072 '��ScrewGet_S3
2073 ''' <summary>
2074 ''' �˂������@����˂��𓾂�
2075 ''' </summary>
2076 '''<param name="%"></param>
2077 '''         PScrewPos(1)    �F�˂�������̂˂����
2078 '''         PScrewPos(2)    �F�˂���������_
2079 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2080 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2081 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2082 '''<returns>����
2083 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
2084 '''</returns>
2085 ''' <remarks>
2086 ''' Date   : 2021/07/07 : M.Hayakawa
2087 ''' </remarks>'
2088 Function M% ScrewGet_S3(ByVal PScrewPosition())
2089     ScrewGet_S3 = 0
2090     MMScrewJudge% = 0
2091     '�˂������평������G���[�`�F�b�N
2092 ' ���b��폜
2093 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
2094 '    Ovrd 100
2095 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
2096 '        Ovrd 30
2097 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
2098 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
2099 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
2100 '        'NG�Ƃ��Ă����̊֐����甲����
2101 '        ScrewGet_S3 = -1
2102 '        MMScrewJudge% = 1
2103 '        MCommentD1001 = 61
2104 '    EndIf
2105 '    If ScrewGet_S3 = 0 Then
2106 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
2107 '        MMScrewJudge% = 0 'MMScrewJudge������������
2108 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2109 '        If MRtn = 0 Then
2110 '            Ovrd 30
2111 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
2112 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
2113 '            MMScrewJudge% = 2
2114 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
2115 '            MCnt% = 2   '2��ݒ�
2116 '            MCommentD1001 = 62
2117 '        EndIf
2118 '        If MMScrewJudge% = 2 Then
2119 '            ScrewGet_S3 = -2
2120 '        EndIf
2121 '    EndIf
2122 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2123 '    If MMScrewJudge% = 2 Then
2124 '        ScrewGet_S3 = -2
2125 '    EndIf
2126     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2127     Ovrd 100
2128     Spd M_NSpd
2129     If MMScrewJudge% = 0 Then
2130         ScrewGet_S3 = 0
2131         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2132         MScrewCnt% = 0
2133         MFinCnt% = 2
2134 '        For MCnt% = 0 To MFinCnt%
2135             Mov PScrewPosition(2)        ' �˂������@���_
2136             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2137             Ovrd 80
2138             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2139             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2140             Mvs PScrewPosition(10), 1.2
2141             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
2142             '�r�b�g��]
2143             M_Out(Y60_Driver)=1
2144             Dly 0.2
2145             '
2146             Ovrd 100
2147             JOvrd M_NJovrd
2148             Spd M_NSpd
2149             '�l�W�z���m�F�ʒu�ړ�
2150             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2151             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2152             '�r�b�g��]��~
2153             'M_Out(Y60_Driver)=0
2154             '
2155             '1�b�ԃl�W�z���m�F
2156 ' �ȉ��b��폜
2157 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2158 '            'MRtn = 0'�����G���[
2159 '            '�z���G���[�̏ꍇ
2160 '            '�l�W���˂����Y�ɖ߂�
2161 '            If MRtn = 0 Then
2162 '                Ovrd 30
2163 '                '�r�b�g��]��~
2164 '                M_Out(Y60_Driver)=0
2165 '                '�l�W�����@���
2166 '                Mvs PScrewPos(1)
2167 '                '�X�ɏ��
2168 '                Mov PScrewPos(1), -75
2169 '                '�l�W�̂Ĉʒu
2170 '                Mov PScrewFeedS021
2171 '                '�z��OFF
2172 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
2173 '                Dly 0.2
2174 '                '�j��ON
2175 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2176 '                '�r�b�g��]
2177 '                M_Out(Y61_Driver)=1
2178 '                Dly 0.5
2179 '                '
2180 '                Ovrd 100
2181 '                JOvrd M_NJovrd
2182 '                Spd M_NSpd
2183 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2184 '                Mov PScrewFeedS021, 10
2185 '                Mov PScrewFeedS021
2186 '                Dly 0.1
2187 '                Mov PScrewFeedS021, 10
2188 '                Mov PScrewFeedS021
2189 '                '
2190 '                '�l�W�����҂�
2191 '                '�r�b�g��]��~
2192 '                M_Out(Y61_Driver)=0
2193 '                Dly 0.1
2194 '                '�j��OFF
2195 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2196 '                '
2197 '                '
2198 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2199 '                Mov PScrewPos(1), -75
2200 '                Ovrd 100
2201 '                Spd M_NSpd
2202 '                '�l�W�����@���
2203 '                Mvs PScrewPos(1)
2204 '                '
2205 '                ScrewGet_S3 = -3
2206 '                Break
2207 '                '
2208 '            Else
2209 '                MCnt% = MFinCnt%
2210 '                ScrewGet_S3 = 0
2211 '            EndIf
2212 '        Next  MCnt%
2213         '
2214         Ovrd 100
2215         Spd M_NSpd
2216         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2217         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2218         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2219         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2220         '������x�z���m�F
2221 ' �ȉ��b��폜
2222 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2223 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2224 '            MCommentD1001 = 94
2225 '            MCommentD1002 = 95
2226 '            ScrewGet_S3 = -3
2227 '        EndIf
2228 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2229 '            ScrewGet_S3 = 1
2230 '        EndIf
2231 '        Break
2232     Else
2233         'M�l�W
2234         If MMScrewJudge% = 2 Then
2235             ScrewGet_S3 = -2
2236         EndIf
2237     EndIf
2238 FEnd
2239 '
2240 '��fnKEY_WAIT()
2241 ''' <summary>
2242 ''' GOT����̃L�[���͑҂�
2243 ''' </summary>
2244 '''<returns>1�F��~    2�F����
2245 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2246 '''         5�FNG
2247 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2248 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2249 '''</returns>
2250 ''' <remarks>
2251 ''' Date   : 2021/07/07 : M.Hayakawa
2252 ''' </remarks>'
2253 Function M% fnKEY_WAIT()
2254     fnKEY_WAIT = 0
2255     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2256     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2257     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2258     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2259     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2260     Dly 0.2
2261     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2262     MLocalLoopFlg=1
2263     While MLocalLoopFlg=1
2264         If M_In(11345) = 1 Then         '��~   M5345
2265             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2266             fnKEY_WAIT = 1
2267             MLocalLoopFlg=-1
2268             Break
2269         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2270             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2271             fnKEY_WAIT = 2
2272             MLocalLoopFlg=-1
2273             Break
2274         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2275             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2276             fnKEY_WAIT = 3
2277             MLocalLoopFlg=-1
2278             Break
2279         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2280             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2281             fnKEY_WAIT = 4
2282             MLocalLoopFlg=-1
2283             Break
2284         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2285             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2286             fnKEY_WAIT = 5
2287             MLocalLoopFlg=-1
2288             Break
2289             '
2290         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2291             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2292             fnKEY_WAIT = MRobotInit1%
2293             MLocalLoopFlg=-1
2294             Break
2295             '
2296         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2297             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2298             fnKEY_WAIT = MRobotInit2%
2299             MLocalLoopFlg=-1
2300             Break
2301             '
2302         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2303             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2304             fnKEY_WAIT = MRobotInit3%
2305             MLocalLoopFlg=-1
2306             Break
2307             '
2308         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2309             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2310             fnKEY_WAIT = MRobotInit4%
2311             MLocalLoopFlg=-1
2312             Break
2313             '
2314         Else
2315         EndIf
2316     WEnd
2317     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2318     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2319 FEnd
2320 '
2321 '�� fnAUTO_CTL
2322 ''' <summary>
2323 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2324 ''' </summary>
2325 ''' <remarks>
2326 ''' Date   : 2021/07/07 : M.Hayakawa
2327 ''' </remarks>
2328 Function M% fnAUTO_CTL
2329     fnAUTO_CTL = 0
2330     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2331     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2332     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2333     '
2334     If M_Svo=0 Then             '�T�[�{ON�m�F
2335         Servo On
2336     EndIf
2337     Wait M_Svo=1
2338 FEnd
2339 '
2340 '�� fnWindScreenOpen
2341 ''' <summary>
2342 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2343 ''' </summary>
2344 '''<param name="%"></param>
2345 '''<param name="%"></param>
2346 '''<param name="%"></param>
2347 '''<param name="%"></param>
2348 ''' <remarks>
2349 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2350 ''' MWindReSet = 0     ��ʔ�\��
2351 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2352 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2353 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2354 ''' Date   : 2021/07/07 : M.Hayakawa
2355 ''' </remarks>
2356 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2357     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2358         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2359     EndIf
2360     '
2361     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2362         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2363     EndIf
2364     '
2365     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2366        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2367     EndIf
2368     '
2369     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2370     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2371     Dly 0.5
2372     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2373 FEnd
2374 '
2375 '��FnCtlValue2
2376 ''' <summary>
2377 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2378 ''' </summary>
2379 ''' <param name="MCtlNo%"></param>
2380 ''' <remarks>
2381 ''' Date : 2022/04/28 �n��
2382 ''' </remarks>
2383 '''
2384 '''  1�F������       �{�P
2385 '''  2�F�g���n�j��   �{�P
2386 '''  3�F�g���m�f��   �{�P (���g�p)
2387 '''  4�F�z���G���[�� �{�P
2388 ''' 99�F�Ǐ��J�n�M�� OFF
2389 '''
2390 Function M% FnCtlValue2(ByVal MCtlNo%)
2391     FnCtlValue2 = 1
2392     Select MCtlNo%
2393         Case 1        '�������{�P
2394             M_Out(12569) = 0             '�����݊J�n�M��OFF
2395             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2396             MInputQty = M_In16(11600)    '��������M
2397             MInputQty = MInputQty + 1    '�������{�P
2398             M_Out16(12592) = MInputQty   '���������M
2399             M_Out(12569) = 1             '�����݊J�n�M��ON
2400             Break
2401             '
2402         Case 2        '�g���n�j���{�P
2403             M_Out(12569) = 0             '�����݊J�n�M��OFF
2404             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2405             MAssyOkQty = M_In16(11616)   '�g��OK����M
2406             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2407             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2408             M_Out(12569) = 1             '�����݊J�n�M��ON
2409             Break
2410             '
2411         Case 4        '�z���G���[���{�P
2412             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2413             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2414             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2415             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2416             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2417             M_Out(12569) = 1                       '�����݊J�n�M��ON
2418             Break
2419             '
2420         Case 99        '�Ǐ��J�n�M��OFF
2421             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2422             M_Out(12569) = 0        '�����݊J�n�M��OFF
2423             Break
2424             '
2425     End Select
2426     Exit Function
2427 FEnd
2428 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2429 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2430 '-------------------------------------------------------------------------------
2431 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2432 '   ����
2433 '       PInspPos()      �F�����ʒu
2434 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2435 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2436 '       MInspCnt%       �F�����ʒu��
2437 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2438 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2439 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2440 '   �߂�l�F����
2441 '       0=�ُ�I���A1=����I��
2442 '
2443 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2444 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2445 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2446 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2447 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2448 '-------------------------------------------------------------------------------
2449     '----- �����ݒ� -----
2450     Cnt 0                                                           '�ړ�����������(�����l=0)
2451     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2452 '    Cnt 1,0.1,0.1
2453     '�ϐ��錾�E������
2454     Def Inte MNum                                                   '�����ԍ�(������1�`)
2455     MNum% = 1                                                       '�����ԍ������l�ݒ�
2456     Def Inte MEndFlg                                                '�����I���t���O
2457     MEndFlg% = 0
2458     '
2459     '����G�ԍ��ݒ�v���E�������s�v��off
2460     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2461     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2462     '�G���[�ԍ��N���A
2463     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2464     M_Out16(MOUT_InspErrNum) = MInspErrNum
2465     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2466     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2467     '
2468     'Insight Ready check?
2469     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2470         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2471         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2472         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2473         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2474         Exit Function
2475     EndIf
2476     '
2477     '�����ʒu���m�F
2478     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2479         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2480         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2481         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2482         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2483         Exit Function
2484     EndIf
2485     '
2486     '
2487     '
2488     '----- ���C������ -----
2489     '�ݒ肳�ꂽ�����ʒu�����̌������s
2490     While( MEndFlg% = 0 )
2491         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2492         MSetGrNumRetryExitFlg = 0
2493         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2494         While( MSetGrNumRetryExitFlg = 0 )
2495         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2496             '
2497             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2498             '
2499             '----- �����O���[�v�ԍ��ݒ� -----
2500             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2501             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2502             '
2503             '�����ʒu�ֈړ��E�ړ������҂�
2504             Mvs PInspPos( MNum% )                                       '�ړ�
2505             Dly 0.05                                                    '�ړ�������Delay
2506             '
2507             '�����O���[�v�ԍ��ݒ�I���m�F
2508             M_Timer(1) = 0
2509             MExitFlg = 0
2510             While( MExitFlg = 0 )
2511                 '����G�ݒ萳��I��?
2512                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2513                     MExitFlg = 1
2514                 '
2515                 '����G�ݒ�ُ�I��?
2516                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2517                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2518                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2519                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2520                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2521                     EndIf
2522                     MExitFlg = 1
2523                 '
2524                 'timeout�`�F�b�N
2525                 ElseIf 1000 < M_Timer(1) Then
2526                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2527                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2528                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2529                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2530                     EndIf
2531                     MExitFlg = 1
2532                 EndIf
2533             WEnd
2534             '
2535             '����G�ԍ��ݒ�v��off
2536             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2537             '
2538             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2539             'NG�Ȃ���Δ�����
2540             If MCurrentStepErr = 0 Then
2541                 MSetGrNumRetryExitFlg = 1
2542             Else
2543                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2544                 If MSetGrNumRetryCnt = 0 Then
2545                     MSetGrNumRetryExitFlg = 1
2546                 Else
2547                     'Retry�ց@���̑O��Delay
2548                     Dly 0.5
2549                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2550                 EndIf
2551             EndIf
2552             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2553             '
2554         WEnd
2555         '
2556         '
2557         '
2558         '----- �������s -----
2559         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2560             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2561                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2562                 MInspRetryExitFlg = 0
2563                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2564                 While( MInspRetryExitFlg = 0 )
2565                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2566                     '
2567                     '���������m�F
2568                     MRetryCnt = MRetryCnt - 1
2569                     M_Timer(1) = 0
2570                     MExitFlg = 0
2571                     While( MExitFlg = 0 )
2572                     '���������҂�
2573                         '����OK�I��?
2574                         If M_In( MIN_IS_InspOK% ) = 1  Then
2575                             MJudgeOKFlg = 1                         '����OK�t���OON
2576                             MExitFlg = 1
2577                         '
2578                         '����NG�I��?
2579                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2580                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2581                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2582                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2583                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2584                                 EndIf
2585                             EndIf
2586                             MExitFlg = 1
2587                         '
2588                         '�����ُ�I��(IS timeout)?
2589                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2590                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2591                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2592                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2593                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2594                                 EndIf
2595                             EndIf
2596                             MExitFlg = 1
2597                         '
2598                         'timeout�`�F�b�N
2599                         ElseIf 3000 < M_Timer(1) Then
2600                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2601                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2602                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2603                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2604                                 EndIf
2605                             EndIf
2606                             MExitFlg = 1
2607                         EndIf
2608                     WEnd
2609                     '
2610                     '�����J�n�v��off
2611                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2612                     '
2613                     'OK�Ȃ甲����
2614                     If MJudgeOKFlg = 1 Then
2615                         MInspRetryExitFlg = 1
2616                     Else
2617                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2618                         If MRetryCnt = 0 Then
2619                             MInspRetryExitFlg = 1
2620                         Else
2621                             'Retry�ց@���̑O��Delay
2622                             Dly 0.3
2623                         EndIf
2624                     EndIf
2625                     '
2626                 WEnd
2627             EndIf
2628         EndIf
2629         '
2630         '
2631         '
2632         MNum% = MNum% + 1                                           '����Step+1
2633         '�����I���m�F�@�����I���t���O�Z�b�g
2634         If (MInspCnt% < MNum% ) Then
2635             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2636         EndIf
2637         'NG���������s������
2638         If MInspErrNum <> 0 Then                                    'NG����?
2639             If MNgContinue% <> 1 Then                               'NG���s?
2640                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2641             EndIf
2642         EndIf
2643     WEnd
2644     '
2645     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2646     If 0 < MZAxis% Then
2647         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2648         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2649         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2650     EndIf
2651     '
2652     '�߂�l�ݒ�
2653     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2654         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2655     Else
2656         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2657         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2658         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2659     EndIf
2660     '
2661     Fine 0,P    'Fine�؂�(22/12/09����)
2662 FEnd
2663 '
2664 ' ��ISInspection
2665 ''' <summary>
2666 ''' Insight�ɂ��摜�����������s
2667 ''' </summary>
2668 '''<param name="PInspPos()">�����ʒu</param>
2669 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2670 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2671 '''<param name="MInspCnt%">�����ʒu��</param>
2672 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2673 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2674 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2675 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2676 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2677 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2678 ''' <remarks>
2679 ''' Date   : 2021/07/07 : M.Hayakawa
2680 ''' </remarks>
2681 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2682 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2683 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2684 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2685 '    EndIf
2686 ''
2687 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2688 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2689 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2690 '    Def Inte MEndFlg                                            '�����I���t���O
2691 '    MEndFlg% = 0
2692 '    '
2693 '    '�G���[�ԍ��N���A
2694 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2695 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2696 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2697 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2698 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2699 '    '
2700 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2701 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2702 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2703 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2704 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2705 ''
2706 '    EndIf
2707 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2708 '    '
2709 '    '�����ʒu���m�F
2710 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2711 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2712 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2713 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2714 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2715 ''
2716 '    EndIf
2717 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2718 '    '
2719 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2720 '    While( MEndFlg% = 0 )
2721 '        '�����I���m�F�@�����I���t���O�Z�b�g
2722 '        If (MInspCnt% < MNum% ) Then
2723 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2724 '        EndIf
2725 '        '
2726 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2727 '        If MEndFlg% = 0 Then
2728 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2729 '        EndIf
2730 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2731 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2732 '        '�^�X�N�@����G�ݒ�t���O���n��
2733 '        If MEndFlg% = 0 Then
2734 '            If 0 < MInspGrNum%(MNum%) Then
2735 '                M_03# = 1
2736 '            Else
2737 '                M_03# = 0
2738 '            EndIf
2739 '        Else
2740 '            M_03# = 0
2741 '        EndIf
2742 '        '�^�X�N�@�������ʊm�F�t���O���n��
2743 '        If 1 < MNum% Then
2744 '            If 0 < MInspGrNum%(MNum%-1) Then
2745 '                M_04# = 1
2746 '            Else
2747 '                M_04# = 0
2748 '            EndIf
2749 '        Else
2750 '            M_04# = 0
2751 '        EndIf
2752 '        '
2753 '        '�^�X�N�����J�n
2754 '        M_00# = 1                                               'TASK�����J�n
2755 '        '�^�X�N�����J�n�m�F
2756 '        M_Timer(1) = 0
2757 '        MExitFlg = 0
2758 '        While( MExitFlg = 0 )
2759 '            '�����J�n�����m�F
2760 '            If M_00# = 0 And M_10# = 8 Then
2761 '                MExitFlg = 1
2762 '            EndIf
2763 '            'timeout�`�F�b�N
2764 '            If 2000 < M_Timer(1) Then
2765 '                If MNgContinue% = 1 Then                        'NG���s?
2766 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2767 '                Else
2768 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2769 '                EndIf
2770 '                MExitFlg = 1
2771 '            EndIf
2772 '        WEnd
2773 '        '
2774 '        '�����ʒu�ֈړ��E�ړ������҂�
2775 '        If 0 = MInspErrNum Then
2776 '            If MEndFlg% = 0 Then
2777 '                Mvs PInspPos( MNum% )                           '�ړ�
2778 '            EndIf
2779 '        EndIf
2780 '        '
2781 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2782 '        If 0 = MInspErrNum Then
2783 '            M_Timer(1) = 0
2784 '            MExitFlg = 0
2785 '            While( MExitFlg = 0 )
2786 '                '���������҂��i����I���j
2787 '                If M_10# = 1 Then
2788 '                    MExitFlg = 1
2789 '                EndIf
2790 '                '���������҂��i�ُ�I���j
2791 '                If M_10# = 0 Then
2792 '                    If MNgContinue% = 1 Then                    'NG���s?
2793 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2794 '                    Else
2795 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2796 '                    EndIf
2797 '                    MExitFlg = 1
2798 '                EndIf
2799 '                'timeout�`�F�b�N
2800 '                If 5000 < M_Timer(1) Then
2801 '                    If MNgContinue% = 1 Then                    'NG���s?
2802 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2803 '                    Else
2804 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2805 '                    EndIf
2806 '                    MExitFlg = 1
2807 '                EndIf
2808 '            WEnd
2809 '        EndIf
2810 '        '
2811 '        '�������ʊm�F
2812 '        If 0 = MInspErrNum Then
2813 '            If 1 < MNum% Then
2814 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2815 '                    If M_11# = 2 Then                           '����NG?
2816 '                        If MNgContinue% = 1 Then                'NG���s?
2817 '                            If MInspNGStepNum = 0 Then          'NG������?
2818 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2819 '                            EndIf
2820 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2821 '                        Else
2822 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2823 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2824 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2825 '                        EndIf
2826 '                   EndIf
2827 '                EndIf
2828 '            EndIf
2829 '        EndIf
2830 '        '
2831 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2832 '        If 0 <> MInspErrNum Then
2833 '            MEndFlg% = 1
2834 '        EndIf
2835 '        '
2836 '        '�������s�A�捞�����҂�
2837 '        If 0 = MInspErrNum Then
2838 '            If MEndFlg% = 0 Then
2839 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2840 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2841 '                    '�捞�����m�F
2842 '                    M_Timer(1) = 0
2843 '                    MExitFlg = 0
2844 '                    While( MExitFlg = 0 )
2845 '                        '���������҂�
2846 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2847 '                            MExitFlg = 1
2848 '                        EndIf
2849 '                        'timeout�`�F�b�N
2850 '                        If 2000 < M_Timer(1) Then
2851 '                            If MNgContinue% = 1 Then            'NG���s?
2852 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2853 '                            Else
2854 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2855 '                            EndIf
2856 '                            MExitFlg = 1
2857 '                        EndIf
2858 '                    WEnd
2859 '                EndIf
2860 '                '
2861 '            EndIf
2862 '        EndIf
2863 '        MNum% = MNum% + 1
2864 '    WEnd
2865 '    '
2866 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2867 '    If 0 < MZAxis% Then
2868 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2869 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2870 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2871 '    EndIf
2872 '    '
2873 '    'NG���s������
2874 '    If MNgContinue% = 1 Then                                    'NG���s?
2875 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2876 '    EndIf
2877 '    '
2878 '    '�߂�l�ݒ�
2879 '    If MInspErrNum = 0 Then
2880 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2881 '    Else
2882 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2883 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2884 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2885 '    EndIf
2886 '    '
2887 '*ISInspection_End
2888 'FEnd
2889 '
2890 '��InitialZoneB
2891 ''' <summary>
2892 ''' ����~��̕��A����
2893 ''' 1)���ޔ��@Z������Ɉړ�
2894 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2895 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2896 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2897 ''' </summary>
2898 ''' <remarks>
2899 ''' Date : 2022/04/08 : N.Watanabe
2900 ''' </remarks>
2901 Function V fnInitialZoneB()
2902     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2903 '
2904 '�p�����[�^
2905     Ovrd 5
2906 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2907 '    Cmp Pos, &B100011
2908 '
2909 '���A����J�n
2910 '
2911 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2912 *RecoveryChuckOpen
2913     PActive = P_Curr          '���݈ʒu���擾
2914     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2915 'PProductOnRoboSet(�˂����{���i�u���ʒu)�́A�`���b�N���
2916     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2917         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2918             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2919                 MRecoveryChuckOpen = 1
2920             EndIf
2921         EndIf
2922     EndIf
2923 'PProductOnRoboGet(�˂����{���i���ʒu)�́A�`���b�N���
2924     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2925         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2926             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2927                 MRecoveryChuckOpen = 1
2928             EndIf
2929         EndIf
2930     EndIf
2931 '
2932 '    If MRecoveryChuckOpen = 1 Then
2933 '        M_Out(12256) = 0        '�{�̃`���b�N��OFF
2934 '        M_Out(12257) = 1        '�{�̃`���b�N�JON
2935 '        M_20# = 0               'KEY���͏�����
2936 '        MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2937 '        If MRtn = 0 Then
2938 '            fErrorProcess(11,244,284,0)
2939 '            If M_20# = MNext% Then M_20# = MClear%
2940 '            If M_20# = MAbout% Then GoTo *RecoveryEnd
2941 '            If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2942 '            If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2943 '        Else
2944 '            M_Out(12257) = 0        '�{�̃`���b�N�JOFF
2945 '        EndIf
2946 '    EndIf
2947 '
2948     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2949     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2950     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2951 '
2952     M_20# = 0                                  'KEY���͏�����
2953     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2954     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2955     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2956 '
2957     fErrorProcess(11,244,284,0)
2958     If M_20# = MNext% Then M_20# = MClear%
2959     If M_20# = MAbout% Then GoTo *RecoveryEnd
2960     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2961     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2962 '
2963     *RecoveryChuckOpenEnd
2964 '
2965 '�w�ʔ��
2966 'PPlateBackSet�`PPlateBackSet_6�̃G���A�ɂ���Ƃ��́A�{�̃`���b�N�J��
2967 '�EPPlateBackSet_6         '�o�H6
2968 '�EPPlateBackSet_5         '�o�H7
2969 '�EPPlateBackSet_4         '�o�H8
2970 '�EPPlateBackSet_3         '�o�H9
2971 '�EPPlateBackSet_2         '�o�H10
2972 '�EPPlateBackSet_1         '�o�H11
2973 '�EPPlateBackSet           '�w�ʔu���ʒu
2974 '��L�V�_�̂w���W�E�x���W�E�y���W��J6�������LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2975     PActive = P_Curr                    '���݈ʒu���擾
2976     JActive = J_Curr                    '���݈ʒu���擾
2977     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2978     If (PActive.X >= -35) And (PActive.X <= -5) Then
2979         If (PActive.Y >= 340) And (PActive.Y <= 515) Then
2980             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2981                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2982                     M_Out(12256) = 0            '�{�̃`���b�N��OFF
2983                     M_Out(12257) = 1            '�{�̃`���b�N�JON
2984                 Dly 1.0
2985                 EndIf
2986             EndIf
2987         EndIf
2988     EndIf
2989 '
2990 '
2991 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2992 '
2993     Ovrd 1
2994 'PProductOnRoboSet(Get)�`PProductOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_2��
2995 '�EPProductOnRoboSet
2996 '�EPProductOnRoboSet_1
2997 '�EPProductOnRoboSet_2
2998 '�EPProductOnRoboGet
2999 '�EPProductOnRoboGet_1
3000 '�EPProductOnRoboGet_2
3001 '��L�U�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
3002     PActive = P_Curr                    '���݈ʒu���擾
3003     JActive = J_Curr                    '���݈ʒu���擾
3004     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
3005     If (PActive.X >= -40) And (PActive.X <= 0) Then
3006         If (PActive.Y >= 380) And (PActive.Y <= 420) Then
3007             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
3008                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3009                     Mvs PProductOnRoboSet_1
3010                     Dly 1.0
3011                     Mvs PProductOnRoboSet_2
3012                     Dly 1.0
3013                     Mov PProductOnRoboSet_3
3014                     Dly 1.0
3015                 EndIf
3016             EndIf
3017         EndIf
3018     EndIf
3019 '
3020 'PProductOnRoboSet(Get)_2�`PProductOnRoboSet(Get)_3�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_3��
3021 '�EPProductOnRoboSet_2
3022 '�EPProductOnRoboSet_3
3023 '�EPProductOnRoboGet_2
3024 '�EPProductOnRoboGet_3
3025 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
3026     PActive = P_Curr                    '���݈ʒu���擾
3027     JActive = J_Curr                    '���݈ʒu���擾
3028     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
3029     If (PActive.X >= -40) And (PActive.X <= 0) Then
3030         If (PActive.Y >= 220) And (PActive.Y <= 420) Then
3031             If (PActive.Z >= 400) And (PActive.Z <= 570) Then
3032                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3033                     Mvs PProductOnRoboSet_3
3034                     Dly 1.0
3035                 EndIf
3036             EndIf
3037         EndIf
3038     EndIf
3039 '
3040     Ovrd 5
3041 '
3042 '���ޔ�
3043     PActive = P_Curr
3044     Pmove = PActive
3045     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
3046     If PActive.X > 550 Then
3047         Pmove.Z =550        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
3048     EndIf
3049     If PActive.Z < Pmove.Z Then
3050         Mvs Pmove
3051     EndIf
3052     Dly 1.0
3053 'J1���ȊO��ޔ��|�W�V�����ֈړ�
3054     JActive = J_Curr
3055     Jmove = JTaihi
3056     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3057     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3058     Mov Jmove
3059     Dly 1.0
3060 'J1���݂̂�ޔ��|�W�V�����ֈړ�
3061     Mov JTaihi
3062     Dly 1.0
3063 '�C�j�V�����|�W�V�����ֈړ�
3064     Mov PInitialPosition
3065     Cmp Off
3066     Ovrd 100
3067 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
3068     If M_In(11856) = 0 Then                 ' ��~���̂�
3069         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
3070         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
3071         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
3072         If MRet = 0 Then
3073         Else
3074             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
3075         EndIf
3076     EndIf
3077     M_Out(12262) = 0            '�ʒu���ߏoOFF
3078     M_Out(12263) = 1            '�ʒu���ߖ�ON
3079     fErrorProcess(11,253,281,0)
3080 *RecoveryEnd
3081     Exit Function
3082 FEnd
3083 '
3084 '
3085 '��fnAutoScreenComment
3086 ''' <summary>
3087 ''' ���C����ʂ̓���󋵕\��
3088 ''' �R�����gD1005�̐ݒ�
3089 ''' </summary>
3090 '''<param name="McommentD1005%">�R�����gID</param>
3091 ''' <remarks>
3092 ''' Date   : 2021/07/07 : M.Hayakawa
3093 ''' </remarks>
3094 Function fnAutoScreenComment(ByVal McommentD1005%)
3095     M_Out16(12576) = McommentD1005%
3096 FEnd
3097 '
3098 '��fnRoboPosChk
3099 ''' <summary>
3100 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
3101 ''' </summary>
3102 '''<param name="MINNumber%">���͔ԍ�</param>
3103 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3104 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3105 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
3106 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
3107 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3108 ''' <remarks>
3109 ''' Date   : 2021/07/07 : M.Hayakawa
3110 ''' </remarks>
3111 Function M% fnRoboPosChk
3112     fnRoboPosChk = 0
3113     MRet = fnStepRead()
3114     '�����ʒu�łȂ��Ɣ��f�����ꍇ
3115     '�E�B���h��ʐ؊���
3116     If MRBTOpeGroupNo > 5 Then
3117         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3118         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3119         Dly 0.2
3120         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3121         Dly 1.5
3122         '
3123         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3124         '
3125         MLoopFlg% = 1
3126         While MLoopFlg% = 1
3127             '
3128             '
3129             MKeyNumber% = fnKEY_WAIT()
3130             Select MKeyNumber%
3131                 Case Is = MAbout%       '��~
3132                     M_20# = MAbout%
3133                     MLoopFlg% = -1
3134                     Break
3135                 Case Is = MNext%        '����
3136                     'MLoopFlg% = -1
3137                     Break
3138                 Case Is = MContinue%    '�p��
3139                     M_20# = MContinue%
3140                     MLoopFlg% = -1
3141                     Break
3142                 Default
3143                     Break
3144             End Select
3145         WEnd
3146     EndIf
3147     '
3148     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3149         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3150         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3151         Select MRBTOpeGroupNo
3152             Case Is = 5                          '�������Ȃ�
3153                 Break
3154             Case Is = 10                         '�����ʒu�֖߂�
3155                 'Mov PTEST001
3156                 Break
3157             Case Is = 15                         '�����ʒu�֖߂�
3158                 'Mov PTEST002
3159                 Dly 0.5
3160                 'Mov PTEST001
3161                 Dly 0.5
3162                 Break
3163             Default
3164                 Break
3165         End Select
3166         '
3167         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3168         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3169         MRBTOpeGroupNo = 5
3170         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3171         Dly 1.0
3172         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3173         fnRoboPosChk = 1                        '�����ʒu������s
3174         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3175     EndIf
3176     Exit Function
3177 FEnd
3178 '
3179 '��frInCheck
3180 ''' <summary>
3181 ''' �Z���T�[IN�`�F�b�N
3182 ''' </summary>
3183 '''<param name="MINNumber%">���͔ԍ�</param>
3184 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3185 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3186 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3187 ''' <remarks>
3188 ''' Date   : 2021/07/07 : M.Hayakawa
3189 ''' </remarks>
3190 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3191     M_Timer(4) = 0
3192     MloopFlg = 0
3193     While MloopFlg = 0
3194         MCrtTime& = M_Timer(4)
3195         If M_In(MINNumber%) = MCMPFLG% Then
3196             MloopFlg = 1
3197             frInCheck = 1
3198         ElseIf MCrtTime& > MTimeCnt& Then
3199             MloopFlg = 1
3200             frInCheck = 0
3201         EndIf
3202     WEnd
3203 FEnd
3204 '-----------------------------------------------
3205 '
3206 '�˂����ߋ@�ʐM�m�F
3207 '
3208 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3209 'fScrewTcomChk = 0�@�F����I��
3210 '          �@ �@ -1 �F�ُ�I��
3211 '-----------------------------------------------
3212 Function M% fScrewTcomChk
3213 *ReCheckScewTcomChk
3214     fScrewTcomChk = 0
3215     '�ʐM�m�F���M
3216     M_Out(MOUT_ScwT_ComChk%) = MOn%
3217     '�ʐM�m�F��M�ҋ@
3218 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3219     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3220     '�ʐM�m�F���M�I��
3221     M_Out(MOUT_ScwT_ComChk%) = MOff%
3222     If MRtn = 0 Then
3223         fScrewTcomChk = -1
3224     EndIf
3225     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3226  '
3227 FEnd
3228 '
3229 '
3230 '-----------------------------------------------
3231 '
3232 '�˂����ߊJ�n���M
3233 '
3234 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3235 'fScrewTStart = 0�@�F����I��
3236 '           �@�@-1 �F�ُ�I��
3237 '-----------------------------------------------
3238 Function M% fScrewTStart
3239     fScrewTStart = 0
3240     nRet% = 0
3241     '�˂����ߊJ�n�ҋ@����M
3242 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3243     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3244     If MRtn = 0 Then nRet% = -1
3245     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
3246     Dly 0.1
3247     '�˂����ߊJ�n��M�𑗐M
3248     M_Out(MOUT_ScwT_ST%) = MOn%
3249     Dly 0.5
3250     'Wait M_In(MTEST_KEY%) = MOn%
3251     '�˂����ߊJ�n���M�I��
3252     M_Out(MOUT_ScwT_ST%) = MOff%
3253     '
3254 *ScrewStartERROR
3255     fScrewTStart = nRet%
3256 FEnd
3257 '
3258 '
3259 '
3260 '-----------------------------------------------
3261 '
3262 '�˂����ߊ�����M
3263 '
3264 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3265 'fScewTFinish = 0�@�F����I��
3266 '          �@ �@-1 �F�ُ�I��
3267 '-----------------------------------------------
3268 Function M% fScewTFinish
3269 *ReCheckScewTFinish
3270     fScewTFinish = 0
3271     '�˂����ߊ����ҋ@����M
3272 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3273     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3274     If MRtn = 0 Then
3275         fScewTFinish = -1
3276     EndIf
3277     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3278     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3279     Dly 0.1
3280     '�˂����ߊ�����M�𑗐M
3281     M_Out(MOUT_ScwT_FinOK%) = MOn%
3282     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3283     '�˂����ߊJ�n���M�I��
3284     M_Out(MOUT_ScwT_FinOK%) = MOff%
3285     'Wait M_In(MTEST_KEY%) = MOn%
3286     '
3287 *ScewTFinish_ErrEnd
3288 FEnd
3289 '
3290 '
3291 '-----------------------------------------------
3292 '
3293 '����xx��~��M
3294 '
3295 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3296 'fScewTCaseStop = 0�@�F����I��
3297 '          �@   �@-1 �F�ُ�I��
3298 '-----------------------------------------------
3299 Function M% fScewTCaseStop(ByVal MCase%())
3300 *ReCheckScewTCaseStop
3301     fScewTCaseStop = 0
3302     '����xx��~����M
3303     Wait M_In(MCase%(1)) = MOn%
3304     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3305     If MRtn = 0 Then
3306         fScewTCaseStop = -1
3307     EndIf
3308     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3309     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3310     Dly 0.1
3311     '����xx��~��M�𑗐M
3312     M_Out(MCase%(2)) = MOn%
3313     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3314     '�˂����ߊJ�n���M�I��
3315     M_Out(MCase%(2)) = MOff%
3316 *ScewTCaseStop_ErrEnd
3317     '
3318 FEnd
3319 '
3320 '��fScrewTighenRoboCheck
3321 '<summary>
3322 '�˂����{�Ď�
3323 '</summary>
3324 '<param name = "MStopNum%"> ��~�ԍ�</param>
3325 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3326 '<make>
3327 '2021/12/2 �����V��
3328 '</make>
3329 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3330     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
3331     fScrewTighenRoboCheck = 1
3332     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3333     MCheck% = 0
3334     While MScrewTighenRoboFlg% = 1
3335         MCheck% = M_In16(11904)
3336         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
3337             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
3338             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
3339         EndIf
3340         If MCheck% <> 0 Then
3341             fScrewTighenRoboError(MCheck%)
3342             Select M_20#
3343                 Case MAbout%            '��~�������ꂽ�ꍇ
3344                     M_Out(12869) = 1 Dly 1.0
3345                     MScrewTighenRoboFlg% = 0
3346                     fScrewTighenRoboCheck = 0   '�ُ�I��
3347                     Break
3348                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3349                     M_Out(12873) = 1 Dly 1.0
3350                     MScrewTighenRoboFlg% = 0
3351                     fScrewTighenRoboCheck = 0   '�ُ�I��
3352                     Break
3353                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3354                     M_20# = MClear%         'M_20#������
3355                     M_Out(12871) = 1 Dly 1.0
3356                     Break
3357                 Case MNext%                 '���ւ������ꂽ�ꍇ
3358                     M_20# = MClear%         'M_20#������
3359                     M_Out(12874) = 1 Dly 1.0
3360                     Break
3361             End Select
3362             Dly 0.5
3363         EndIf
3364     WEnd
3365 FEnd
3366 '
3367 '��fScrewTighenRoboError
3368 '<summary>
3369 '�˂����{�G���[����
3370 '</summary>
3371 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3372 '<make>
3373 '2021/12/2 �����V��
3374 '</make>
3375 Function fScrewTighenRoboError(ByVal MErrorCode%)
3376     MErrorScreenCode% = 0
3377     MErrorScreenCode% = MErrorCode% + 300
3378     fErrorProcess(11,MErrorScreenCode%,0,0)
3379 FEnd
3380 '
3381 '��fErrorProcess
3382 '<summary>
3383 '�G���[����
3384 '</summary>
3385 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3386 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3387 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3388 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3389 '<make>
3390 '2021/11/5 �����V��
3391 '</make>
3392 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3393     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3394     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3395     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3396     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3397 *RETRY_ERR_PROCESS
3398      M_20# = MClear%     '������
3399 '        '�G���[�����L�q
3400         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3401 '        'GOT KEY���͑҂�
3402         MKeyNumber = fnKEY_WAIT()
3403 '        '
3404         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3405             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3406             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3407             Break
3408          '
3409         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3410             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3411             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3412         '
3413         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3414             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3415             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3416          '
3417         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3418             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3419             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3420             Break
3421         '
3422         EndIf
3423         '
3424         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3425 FEnd
3426 '
3427 '��fnTorqueCheck
3428 ''' <summary>
3429 ''' �g���N�`�F�b�N����p�̃��C��
3430 ''' </summary>
3431 ''' <remarks>
3432 ''' Date   : 2021/12/21 : H.AJI
3433 ''' </remarks>'
3434 Function M% fnTorqueCheck
3435     '�g���N�`�F�b�N�����M  �����n��~
3436     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3437     '
3438     fnTorqueCheck = 0
3439     Ovrd 20
3440     Mov PInitialPosition              '�����ʒu�ړ�
3441     Ovrd 100
3442     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3443     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3444     Dly 0.2
3445     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3446     '
3447     'M6340  �g���N�`�F�b�N��M
3448     'Dly 5.0
3449     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3450     Dly 1.0
3451     M_Out(12340) = 0
3452     '
3453     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3454     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3455    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3456     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3457     '
3458     '
3459     MLoopFlg = 1
3460     While MLoopFlg = 1
3461         '
3462         Mov PInitialPosition              '�����ʒu�ړ�
3463         '
3464         MKeyNumber = fnKEY_WAIT()
3465         Select MKeyNumber
3466             Case Is = 1           '��~
3467                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3468                 Dly 1.0
3469                 M_Out(12343) = 0
3470                 Ovrd 20
3471                 'Mov PTicketRead_1
3472                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3473                 Wait M_In(11859) = 1      '�˂����{����̏I��
3474                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3475                 Ovrd 100
3476                 M_20# = 1
3477                 MLoopFlg = -1
3478                 Break
3479             Case Is = 2           '����
3480                 Break
3481             Case Is = 3           '�p��
3482                 Break
3483             Case Is = 4           '�g���N�`�F�b�N�J�n
3484                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3485                 Dly 1.0
3486                 M_Out(12342) = 0
3487                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3488                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3489                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3490                 EndIf
3491                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3492                 'MRet = fnMoveTorquePosi()
3493                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3494                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3495                 Break
3496             Default
3497                 Break
3498         End Select
3499     WEnd
3500     '
3501     '�g���N�`�F�b�N����~���M
3502     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3503     '
3504     '���{�b�g�̈ʒu�����ɖ߂�
3505     '
3506     '
3507  FEnd
3508  '
3509 '
3510 '
3511 '---------------------------
3512 '
3513 '    ���C����ʂ̕\���A��\���ݒ�
3514 '         �R�����gD1001, D1002, D1003�̐ݒ�
3515 '           MWindReSet = 0     ��ʔ�\��
3516 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3517 '           MWindErrScr = 10    �G���[��� D1001, D1002
3518 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3519 '
3520 '---------------------------
3521 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3522     fnMainScreenOpen = 0
3523     '
3524    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3525         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3526     EndIf
3527     '
3528     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3529         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3530     EndIf
3531     '
3532     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3533         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3534     EndIf
3535     '
3536     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3537     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3538     Dly 0.5
3539     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3540 FEnd
3541 '
3542 '��Main
3543 ''' <summary>
3544 ''' �g���N�`�F�b�N������
3545 ''' </summary>
3546 ''' <remarks>
3547 ''' Date   : 2021/12/21 : H.AJI
3548 ''' </remarks>'
3549 Function M% fnScrewMTorque
3550     fnScrewMTorque = 0
3551     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3552     Wait M_In(11857) = 1                     '��M����
3553     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3554     Dly 2.0
3555 FEnd
3556 '
3557 '��Main
3558 ''' <summary>
3559 ''' �g���N�`�F�b�N������
3560 ''' </summary>
3561 ''' <remarks>
3562 ''' Date   : 2021/12/21 : H.AJI
3563 ''' </remarks>'
3564 Function M% fnMoveTorquePosi
3565      fnMoveTorquePosi = 0
3566      Ovrd 50
3567     'Mov PTorquePosi000 '�g���N�`�F�b�N����ʒu�ֈړ�
3568      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
3569     'Mov PTorquePosi020 '�g���N�`�F�b�N�r�b�g�W���C���g���
3570     '
3571     '
3572      '�ȉ��͈������񂪍쐬�����g���N�`�F�b�N�v���O����
3573     '
3574     Spd M_NSpd
3575 '-------------      �h���C�o�[RST
3576     M_Out(12240)=0     '�h���C�o�[OFF CCW
3577     M_Out(12241)=0     '�h���C�o�[OFF CW
3578     M_Out(12242)=0     '�h���C�o�[���� C1
3579     M_Out(12243)=0     '�h���C�o�[���� C2
3580     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
3581 '---------------------------------------
3582 '---------------------------------------
3583     Fsc Off            '�͊o�Z���T�@Off  STEP1�͕s�v
3584 '--------------------------------------------------------------
3585 '--------------------------------------------------------------
3586 '[P-11]
3587 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
3588     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
3589    'Mov PTorquePosi020, -10                    ' �g���N-1�@�u���ʒu��� 10mm �ֈړ�
3590     Dly 0.1
3591 '-----------------------
3592    'Cnt 0                           'Cnt����-2�@�I��
3593 '-----------------------
3594     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
3595     Dly 0.2
3596 '-----------------------
3597     M_Out(12242)=1                   '�h���C�o�[�Z�b�g C1
3598     Dly 0.1
3599     M_Out(12243)=1                   '�h���C�o�[�Z�b�g C2 (�o���N3)
3600     Dly 0.1
3601     M_Out(12245)=1                   '�v���O����2�Z�b�g F1  M�l�W
3602     Dly 0.1
3603     'M_Out(12241)=1                   '�h���C�o�[ON  CW
3604    M_Out(12241)=0                   '�h���C�o�[OFF  CW
3605     'Dly 0.1
3606 '--------------------------------
3607     Ovrd 40
3608    'Dly 0.1
3609 '--------------------------------  �l�W���ߑ��x�ݒ�
3610     Spd 14                            '���C�h 100-40 100% :Spd 12
3611     Dly 0.1
3612 '--------------------------------
3613 '--------------------------------
3614 '---------------------------------�y�˂����ߓ���z
3615 '
3616     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
3617    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
3618     Dly 0.3                          '�������҂�
3619    M_Out(12241)=1                   '�h���C�o�[ON  CW
3620 '
3621     Wait M_In(11584)=1                '����/�G���[���o
3622     Dly 0.1
3623     Spd M_NSpd
3624    'Ovrd 20
3625     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
3626     Wait M_In(11257)=1                '�l�W����SC
3627 '---------------------------------
3628     Dly 0.1
3629     M_Out(12241)=0                    '�h���C�o�[OFF CW
3630     Dly 0.1
3631     M_Out(12242)=0                    '�h���C�o�[���� C1
3632     Dly 0.1
3633     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
3634     Dly 0.1
3635     M_Out(12245)=0                    '�v���O����2���� F1
3636 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
3637 '
3638     Mvs PTorqueCheck,-60                       '������mov ����ύX
3639     Dly 0.1
3640 '--------------------------------------------------------------
3641    'Ovrd 80
3642 '--------------------------------------------------------------
3643 '---------------------------------------
3644 '---------------------------------------
3645 '---------------------------------------�G���[���E����
3646    *LBL1
3647    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
3648    Mvs ,-100
3649    M_Out(12241)=0     '�h���C�o�[OFF CW
3650    Dly 0.1
3651    M_Out(12242)=0     '�h���C�o�[���� C1
3652    Dly 0.1
3653    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
3654    Dly 0.1
3655    M_Out(12245)=0     '�v���O�������� F1
3656 '---------------------------------------
3657 '---------------------------------------
3658 '-------------
3659    'Mov PInitPos19049
3660    Dly 0.1
3661 '
3662 '
3663 '
3664 '
3665 FEnd
3666 '
3667 '
3668 '----------------------------------------------------------------
3669 'fTimeOutJudge
3670 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3671 '����
3672 'Address% = �Ď��A�h���X�ԍ�
3673 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3674 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3675 '�߂�l = 0 �G���[
3676 '         1 ����I��
3677 '         2 ���g���C
3678 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3679 '�쐬��
3680 '2022/9/20 ����
3681 '----------------------------------------------------------------
3682 '
3683 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3684     fTimeOutJudge = 0
3685     MJudge% = 1
3686     MRtn = 0
3687     M_20# = MClear%
3688     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3689 *TimeOutLoop
3690     If MRtn = 1 Then GoTo *TimeOut
3691         fErrorProcess(11,202,203,0)
3692         If M_20# = MNext% Then GoTo *TimeOutLoop
3693         If M_20# = MContinue% Then MJudge% = 2
3694         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3695 *TimeOut
3696     fTimeOutJudge = MJudge%
3697 '
3698 *JUDGE_ERROR_END
3699 FEnd
3700 '��Main
3701 ''' <summary>
3702 ''' �g������p�̃��C��
3703 ''' </summary>
3704 ''' <remarks>
3705 ''' Date   : 2021/07/07 : M.Hayakawa
3706 ''' </remarks>'
3707 Function Main
3708     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3709     '
3710     If M_Svo=0 Then
3711         Servo On
3712     EndIf
3713     Wait M_Svo=1
3714 '�g���X�^�[�g���t�����v���p���XON
3715     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3716 '�p�g���C�g����
3717     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3718     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3719     '
3720     M_20# = 0                                   'KEY���͏�����
3721     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3722     MRet% = 0
3723 '�����ʒu�̊m�F�ƈړ�
3724 '
3725 '���A����@���s�E�����s����      2022/04/08 �n�� �쐬
3726     PActive = P_Curr                    '���݈ʒu���擾
3727     MRecoveryPass% = 0
3728     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3729         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3730             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3731                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3732             EndIf
3733         EndIf
3734     EndIf
3735     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3736         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3737             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3738                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3739             EndIf
3740         EndIf
3741     EndIf
3742     If MRecoveryPass% = 0 Then
3743        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3744     EndIf
3745 '
3746 '
3747 '    MRet% = fnRoboPosChk()
3748 '    If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ    '2022/04/26 �R�����g�A�E�g �n��
3749 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3750 '        MKeyNumber% = fnKEY_WAIT()
3751 '        Select MKeyNumber%
3752 '            Case Is = MAbout%       '��~
3753 '                M_20# = MAbout%
3754 '                MLoopFlg% = -1
3755 '                Break
3756 '            Case Is = MNext%        '����
3757 '                'MLoopFlg = -1
3758 '                Break
3759 '            Case Is = MContinue%    '�p��
3760 '                M_20# = MContinue%
3761 '                MLoopFlg% = -1
3762 '                Break
3763 '            Default
3764 '                Break
3765 '        End Select
3766 '    EndIf
3767     '
3768     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3769         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3770 '�g���N�`�F�b�N
3771         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3772             MRet% = fnTorqueCheck()
3773             Break
3774         Else
3775 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3776 '                MRtn = InspInit()               '�摜��������������
3777 '            EndIf
3778             '
3779            M_20# = MClear%                    '������
3780 '�g���J�n
3781             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3782                 fnAssyStart()
3783             Else
3784                 M_20# = MPass%
3785             EndIf
3786 '�g���I�����t����
3787             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3788             Wait M_In(11572) = 1            '���t�擾����
3789             Dly 0.1
3790             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3791 '���t�^�[���j�b�g�ւ�OUT
3792             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3793             fnAutoScreenComment(89)         'AUTO��� �g����������
3794             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3795 'OK/NG�t���O�o��
3796             If M_20# <= 0 Then
3797                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3798             ElseIf M_20# = MPass% Then
3799                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3800             EndIf
3801 'PIAS�ɑg������������
3802             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3803                 If M_20# = MPass% Then
3804                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3805                 Else
3806                     'KEY���͂�NG�̏ꍇ
3807                     If M_20# = MNgProcess% Then
3808                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3809                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3810                         MRet% = fnPiasWrite(MNG%)
3811                        nAssyNgQty = nAssyNgQty + 1
3812                     EndIf
3813                     '
3814                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3815                     If M_20# = MAssyOK% Then
3816                             '-----------------------
3817                             'D732 -> D2600 �R�s�[�v��
3818                             M_Out(12566) = 1
3819 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3820                             M_Out(12566) = 0
3821                             '
3822                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3823                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3824                             '��ԍ��ƍ�(PP�͖��g�p�j
3825 '                            MRet% = fnPCBNumberCheck()
3826                         Else
3827                             MRet% = 1
3828                         EndIf
3829                         '
3830                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3831                             If M_20# <> MAbout% Then
3832                                 '�H������OK��������
3833                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3834                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3835                                 MRet% = fnPiasWrite(MOK%)
3836                                 nAssyOkQty = 0
3837                                 nAssyOkQty = nAssyOkQty + 1
3838                             Else
3839                                 nAssyOkQty = nAssyOkQty + 1
3840                             EndIf
3841                         EndIf
3842                     EndIf
3843 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3844 '                    MRet% = fnPiasWrite(MOK%)
3845                 EndIf
3846             Else
3847                 nAssyOkQty = nAssyOkQty + 1
3848             EndIf
3849             '
3850             '�g���I�����t��������
3851             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3852             '�������A�g��OK���A�g��NG��������
3853 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3854             '
3855 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3856 '                '�摜�����I������
3857 '                MRtn = InspQuit()
3858 '            EndIf
3859         EndIf
3860         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3861     EndIf
3862 '�p�g���C�g����
3863     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3864     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3865 'GOT�\��
3866     fnAutoScreenComment(93)  'AUTO��� �H������
3867 FEnd
3868 End
3869 '
3870 '���܂��Ȃ��R�����g
3871 '��΍폜�����
3872 '
3873 '
3874 '
3875 '
3876 '
3877 '
3878 '
3879 '
3880 '
JActive=(-11.550,19.840,77.070,0.070,83.290,-10.420,0.000,0.000)
Jmove=(-11.550,-46.870,111.640,0.000,80.580,-10.420,0.000,0.000)
JTaihi=(0.000,-46.870,111.640,0.000,80.580,0.000)
PActive=(340.000,0.000,580.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInitialPosition=(340.000,0.000,580.000,-180.000,0.000,180.000)(7,0)
PMechaGet=(-415.660,-8.740,299.320,179.450,-2.220,176.860)(7,1048577)
PMechaGet_1=(-415.660,-8.740,409.960,179.450,-2.220,176.860)(7,1048577)
PMechaGet_2=(-189.840,-0.010,629.060,-180.000,0.000,-179.990)(7,1)
PMechaGet_3=(0.010,189.840,629.070,-180.000,0.000,90.000)(7,0)
PMechaGet_4=(327.500,0.020,596.240,-179.990,0.000,103.500)(7,0)
PMechaGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet1=(159.800,-334.910,320.000,122.020,86.640,31.390)(6,0)
PMechaSet1_1=(159.800,-334.910,340.000,122.020,86.640,31.390)(6,0)
PMechaSet2=(160.190,-334.970,320.180,122.020,86.640,31.360)(6,0)
PMechaSet2_1=(160.190,-334.970,339.970,122.020,86.640,31.360)(6,0)
PMechaSet_2=(162.580,-305.370,557.380,179.470,90.000,89.470)(6,0)
PMechaSet_3=(114.450,-288.220,565.580,180.000,0.000,112.110)(7,0)
PMechaSet_4=(310.110,-0.040,565.560,180.000,0.000,-179.550)(7,0)
PMechaSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
Pmove=(478.560,-97.670,640.000,179.900,-0.170,178.870,0.000,0.000)(7,0)
PPlateBackCheck=(-74.490,309.560,598.750,180.000,-54.150,90.000)(7,1048576)
PPlateBackCheck_2=(-59.410,338.490,632.860,170.410,-66.660,89.130)(7,1048576)
PPlateBackCheck_3=(-17.860,286.220,630.900,-179.690,-0.410,90.870)(7,1048576)
PPlateBackCheck_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet=(469.500,105.410,400.260,179.920,-0.070,179.170)(7,0)
PPlateBackGet_1=(469.490,105.410,430.000,179.920,-0.070,179.170)(7,0)
PPlateBackGet_2=(469.490,105.410,560.000,179.920,-0.070,179.170)(7,0)
PPlateBackGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackPush=(-19.270,414.740,537.150,179.810,0.000,88.900)(7,1048576)
PPlateBackPush_1=(-19.270,405.000,537.150,179.810,0.000,88.900)(7,1048576)
PPlateBackPush_2=(-19.270,405.000,546.600,179.810,0.000,88.470)(7,1048576)
PPlateBackSet=(-19.270,455.480,538.130,-179.890,-11.000,88.450)(7,1048576)
PPlateBackSet_1=(-19.270,449.080,536.520,-179.830,-13.000,88.440)(7,1048576)
PPlateBackSet_10=(-19.010,351.550,478.290,-178.900,-45.000,88.440)(7,1048576)
PPlateBackSet_11=(-19.010,348.410,478.290,-178.900,-45.000,88.440)(7,1048576)
PPlateBackSet_12=(-19.010,346.190,488.960,-178.900,-45.000,88.440)(7,1048576)
PPlateBackSet_13=(-17.860,286.220,630.900,-179.690,-0.410,90.870)(7,1048576)
PPlateBackSet_2=(-19.270,435.190,532.920,-179.720,-17.000,88.440)(7,1048576)
PPlateBackSet_3=(-18.970,422.190,528.410,-179.610,-21.000,88.440)(7,1048576)
PPlateBackSet_4=(-18.970,408.850,522.580,-179.490,-25.000,88.440)(7,1048576)
PPlateBackSet_5=(-18.970,396.670,516.380,-179.390,-29.000,88.440)(7,1048576)
PPlateBackSet_6=(-18.970,385.090,508.310,-179.280,-33.000,88.440)(7,1048576)
PPlateBackSet_7=(-18.970,373.630,499.600,-179.170,-37.000,88.440)(7,1048576)
PPlateBackSet_8=(-18.970,362.940,490.670,-179.040,-41.000,88.440)(7,1048576)
PPlateBackSet_9=(-18.970,353.140,480.980,-178.900,-45.000,88.440)(7,1048576)
PProductOnPltGet=(478.760,-97.710,372.280,179.900,-0.170,179.390)(7,0)
PProductOnPltGet_1=(478.760,-97.710,410.000,179.900,-0.170,-179.100)(7,0)
PProductOnPltGet_2=(478.760,-97.710,500.000,179.900,-0.170,-179.100)(7,0)
PProductOnPltGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet=(477.080,-96.540,372.590,179.900,-0.170,178.770)(7,0)
PProductOnPltSet_1=(477.080,-96.540,410.000,179.900,-0.170,178.770)(7,0)
PProductOnPltSet_2=(477.080,-96.540,500.000,179.900,-0.170,178.770)(7,0)
PProductOnPltSet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet=(-18.540,403.100,319.090,87.030,88.570,177.170)(6,0)
PProductOnRoboGet_1=(-18.540,403.100,410.000,87.030,88.570,177.170)(6,0)
PProductOnRoboGet_2=(-18.540,396.290,419.810,84.410,89.140,174.550)(6,0)
PProductOnRoboGet_3=(-19.740,378.130,425.480,-105.620,89.690,-15.620)(6,0)
PProductOnRoboGet_4=(-19.740,300.000,550.000,175.040,89.990,-94.950)(6,0)
PProductOnRoboGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet=(-18.540,403.200,319.090,87.030,88.570,177.170)(6,0)
PProductOnRoboSet_1=(-18.540,403.200,410.000,87.030,88.570,177.170)(6,0)
PProductOnRoboSet_2=(-18.540,396.290,419.810,84.410,89.140,174.550)(6,0)
PProductOnRoboSet_3=(-18.910,236.650,555.850,-94.470,89.060,-4.460)(6,0)
PProductOnRoboSet_4=(-18.860,404.690,360.000,-102.740,89.080,-12.360)(6,0)
PProductOnRoboSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPushTilt=(-213.930,562.960,465.170,179.970,-0.020,4.010)(7,0)
PPushTilt_1=(-213.930,562.970,480.780,179.960,-0.020,4.010)(7,0)
PPushTilt_2=(-213.930,562.960,620.000,179.970,-0.020,4.010)(7,0)
PPushTilt_3=(0.020,340.000,610.000,-180.000,-0.010,-91.910)(7,0)
PTemp=(340.000,0.000,580.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PTicketRead=(602.000,-150.000,500.000,180.000,0.000,90.000)(7,0)
PTicketRead_1=(602.000,-150.000,550.000,180.000,0.000,90.000)(7,0)
PTicketRead_2=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(1)=(-74.490,309.560,598.750,180.000,-54.150,90.000,0.000,0.000)(7,1048576)
PInspPosition(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(11)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(12)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(13)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(14)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(15)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(16)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(17)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(18)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(19)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(20)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(21)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(22)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(23)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(24)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(25)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(26)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(27)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(28)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(29)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(30)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
