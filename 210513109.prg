1 ' ===================================
2 '
3 '  2100301001 STEP5 Assy1�v���O����
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
43 Def Inte MovrdA                     '�l�W����Ovrd �ϗp
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
207 MRtn% = 0
208 MRet = 0
209 MRet3% = 0
210 '
211 Def Inte MInputQty          '������ ���Z�ϐ�
212 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
213 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
214 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
215 Def Inte nAssyOkQty         '���g�p
216 Def Inte MScrewNo
217 Def Inte MReTry
218 '===== <IO�ϐ���`> =====
219 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
220 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
221 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
222 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
223 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
224 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
225 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
226 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
227 '
228 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
229 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
230 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
231 '
232 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
233 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
234 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
235 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
236 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
237 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
238 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
239 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
240 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
241 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
242 '
243 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
244 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
245 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
246 '
247 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
248 '
249 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
250 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
251 '
252 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
253 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
254 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
255 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
256 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
257 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
258 '
259 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
260 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
261 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
262 '
263 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
264 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
265 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
266 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
267 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
268 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
269 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
270 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u '���l12250����12248�֕ύX(8/5����)
271 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
272 MOUT_VB1%   =  12250    ' �A�[����[�@�l�W�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
273 '
274 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
275 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
276 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
277 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
278 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
279 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
280 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
281 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
282 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
283 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
284 '
285 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
286 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
287 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
288 '
289 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
290 '
291 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
292 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
293 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
294 '
295 '����
296 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
297 Def Inte MOn                            '�o��=1
298 Def Inte MOff                           '�o��=0
299 '
300 '�˂����ߑ��u_�o�̓A�h���X
301 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
302 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
303 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
304 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
305 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
307 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
308 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
309 '�˂����ߑ��u_���̓A�h���X
310 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
311 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
312 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
313 Def Inte MIN_ScwT_Case1                 '����1��~����M
314 Def Inte MIN_ScwT_Case2                 '����2��~����M
315 Def Inte MIN_ScwT_Case3                 '����3��~����M
316 Def Inte MIN_ScwT_Case4                 '����4��~����M
317 Def Inte MIN_ScwT_Case5                 '����5��~����M
318 '
319 Dim MScwT_Case1%(2)               '����1��~�ϐ�
320 Dim MScwT_Case2%(2)               '����2��~�ϐ�
321 Dim MScwT_Case3%(2)               '����3��~�ϐ�
322 Dim MScwT_Case4%(2)               '����4��~�ϐ�
323 Dim MScwT_Case5%(2)               '����5��~�ϐ�
324 '
325 '����
326 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
327 MOn% = 1                                 '�o�� = 1
328 MOff% = 0                                '�o�� = 0
329 '
330 '�˂����ߋ@_�A�h���X�ݒ�
331 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
332 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
333 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
334 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
335 MOUT_ScwT_Case1OK% = 12874              '����1��~��M�𑗐M
336 MOUT_ScwT_Case2OK% = 12875              '����2��~��M�𑗐M
337 MOUT_ScwT_Case3OK% = 12876              '����3��~��M�𑗐M
338 MOUT_ScwT_Case4OK% = 12877              '����4��~��M�𑗐M
339 MOUT_ScwT_Case5OK% = 12878              '����5��~��M�𑗐M
340 '
341 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
342 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
343 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
344 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
345 MIN_ScwT_Case1% = 11882                 '����1��~�ҋ@����M
346 MIN_ScwT_Case2% = 11883                 '����2��~�ҋ@����M
347 MIN_ScwT_Case3% = 11884                 '����3��~�ҋ@����M
348 MIN_ScwT_Case4% = 11885                 '����4��~�ҋ@����M
349 MIN_ScwT_Case5% = 11886                 '����5��~�ҋ@����M
350 '
351 MScwT_Case1%(1) = MIN_ScwT_Case1%
352 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
353 MScwT_Case2%(1) = MIN_ScwT_Case2%
354 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
355 MScwT_Case3%(1) = MIN_ScwT_Case3%
356 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
357 MScwT_Case4%(1) = MIN_ScwT_Case4%
358 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
359 MScwT_Case5%(1) = MIN_ScwT_Case5%
360 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
361 '
362 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
363 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
364 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
365 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
366 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
367 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
368 Def Inte MRecoveryPass      '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
369 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
370 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
371 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
372 '
373 '
374 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
375 Function M% fnAssyStart
376     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/27 �n��
377     M_20# = MClear%                       '������
378 '
379 '    fScewTStart()           '�����ʒu�ύX
380 ' �g���J�n
381 '�v���O�������_            '(�ǉ���������(8/30����))
382 Ovrd 100
383 '
384 '�n���h�ɖ{��,�����Ȃ����m�F
385 *RE_INITIAL_CHECK
386 If M_20# = MContinue% Then M_20# = MClear%
387 '
388 If M_In(11264) = 0 Then GoTo *CompInitial_1
389 fErrorProcess(11,253,281,0)
390 If M_20# = MNext% Then M_20# = MClear%
391 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
392 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
393 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
394 *CompInitial_1
395 '
396 If M_In(11267) =0 And M_In(11272)= 0 Then GoTo *CompInitial_2
397 fErrorProcess(11,255,281,0)
398 If M_20# = MNext% Then M_20# = MClear%
399 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
400 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
401 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
402 *CompInitial_2
403 '
404 '�n���h���C�j�V�����ɖ߂�
405 If M_In(11266) = 1 Then             '�{�̃`���b�N���o
406     M_Out(12256) = 0                '�{�̃`���b�N��OFF
407     M_Out(12257) = 1                '�{�̃`���b�N�JON
408     Break
409 EndIf
410 If M_In(11269) = 1 Then             '����L�V�����_�[�o���o
411     M_Out(12258) = 0                '����L�V�����_�[�oOFF
412     M_Out(12259) = 1                '����L�V�����_�[��ON
413     Break
414 EndIf
415 If M_In(11274) = 1 Then             '����R�V�����_�[�o���o
416     M_Out(12260) = 0                '����R�V�����_�[�oOFF
417     M_Out(12261) = 1                '����R�V�����_�[��ON
418     Break
419 EndIf
420 M_Out(12262) = 0                    '���`���b�N��OFF
421 M_Out(12263) = 1                    '���`���b�N�JON
422 '
423 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)'�{�̃`���b�N�J���o
424 If MRtn = 1 Then GoTo *CompInitial_3
425 fErrorProcess(11,244,281,0)
426 If M_20# = MNext% Then M_20# = MClear%
427 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
428 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
429 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
430 *CompInitial_3
431 '
432 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)'����L�ߌ��o
433 If MRtn = 1 Then GoTo *CompInitial_4
434 fErrorProcess(11,247,281,0)
435 If M_20# = MNext% Then M_20# = MClear%
436 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
437 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
438 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
439 *CompInitial_4
440 '
441 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)'����R�ߌ��o
442 If MRtn = 1 Then GoTo *CompInitial_5
443 fErrorProcess(11,249,281,0)
444 If M_20# = MNext% Then M_20# = MClear%
445 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
446 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
447 If M_20# = MContinue% Then GoTo *RE_INITIAL_CHECK
448 *CompInitial_5
449 '
450 '�����ʒu��ݒ�
451 PTemp = P_Curr
452 MRtn = 0
453 'If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
454 '    If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
455 '        If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
456 '            MRtn = 1
457 '            Break
458 '        EndIf
459 '        Break
460 '    EndIf
461 '    Break
462 'EndIf
463 'If MRtn = 1 Then
464 '    M_Out(12265) = 0            '�ʒu���ߖ�OFF
465 '    M_Out(12264) = 1            '�ʒu���ߏoON
466 '    Mov PTicketRead
467 '    Break
468 'Else
469 '    Mov PInitialPosition
470 '    M_Out(12265) = 0            '�ʒu���ߖ�OFF
471 '    M_Out(12264) = 1            '�ʒu���ߏoON
472 '    Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
473 '    Mvs PTicketRead             'ID�ǂ݈ʒu
474 '    Break
475 'EndIf
476 '
477 ' 2022/04/12 ���S�����֏����ύX �n��
478 ' PInitialPosition �ݐ� MStandby=2
479 ' PTicketRead_1 �ݐ� MStandby=1
480 '
481 MStandby = 0    '�ҋ@�ʒu�t���O��������
482 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
483     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
484         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
485             MStandby = 2
486         EndIf
487     EndIf
488 EndIf
489 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
490     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
491         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
492             MStandby = 1
493         EndIf
494     EndIf
495 EndIf
496 If MStandby = 2 Then
497     M_Out(12265) = 0            '�ʒu���ߖ�OFF
498     M_Out(12264) = 1            '�ʒu���ߏoON
499     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
500     Mvs PTicketRead             'ID�ǂ݈ʒu
501     Break
502 EndIf
503 If MStandby = 1 Then
504     M_Out(12265) = 0            '�ʒu���ߖ�OFF
505     M_Out(12264) = 1            '�ʒu���ߏoON
506     Mvs PTicketRead             'ID�ǂ݈ʒu
507     Break
508 EndIf
509 If MStandby <> 0 Then GoTo *PositionOK
510 fErrorProcess(11,230,281,0)           '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
511 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
512 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
513 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
514 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
515 *PositionOK
516 '
517 MRtn = 1        'MRtn������
518 '�`�P�b�gID��ǂ�
519 *RE_TICKET_READ
520 If M_20# = MContinue% Then M_20# = MClear%
521 'PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
522 'MInspGroup%(1) = 1              '����G�ԍ�
523 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
524 'M_20# = MClear%                       '������
525 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
526     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
527     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
528     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
529 EndIf
530 If MRtn = 1 Then GoTo *CompRead
531 'fErrorProcess(11,244,284,0)
532 If M_20# = MNext% Then M_20# = MClear%
533 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
534 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
535 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
536 'If M_20# = MNext% Then M_20# = MPass%
537 Mov PTicketRead_1                        '�G���[�I�����C�j�V�����ɖ߂�
538 Mov PInitialPosition
539 GoTo *ASSY_ERROR_END
540 *CompRead
541 '
542 Mov PTicketRead_1               '�`�P�b�gID�ǂݎ����_
543 'Dly 5                   '�f�o�b�O�p(22/09/30����)
544 '    '�˂����ߊJ�n(�����ʒu�ύX2/16����)
545     MRtn2 = fScewTStart()
546     If MRtn2 = 0 Then GoTo *INITIAL_CHECK
547         fErrorProcess(11,329,201,0)
548         If M_20# = MNext% Then GoTo *CompRead
549         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
550         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
551         If M_20# = MContinue% Then GoTo *CompRead
552 '
553 '
554 *INITIAL_CHECK
555 '
556 '
557 '�p���b�g���琻�i�����
558 Mov PProductOnPltGet_2      '�{�̉��_
559 *RE_PLT_GET_1
560 If M_20# = MContinue% Then M_20# = MClear%
561 '
562 M_Out(12256) = 0            '�{�̃`���b�N��OFF
563 M_Out(12257) = 1            '�{�̃`���b�N�JON
564 '
565 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '�{�̃`���b�N�J�Z���T�[ON
566 If MRtn = 1 Then GoTo *CompPltGet_1
567 fErrorProcess(11,244,284,0)
568 If M_20# = MNext% Then M_20# = MClear%
569 If M_20# = MAbout% Or M_20# = MNgProcess% Then
570     Mov PInitialPosition
571     M_Out(12264) = 0            '�ʒu���ߏoOFF
572     M_Out(12265) = 1            '�ʒu���ߖ�ON
573     Break
574 EndIf
575 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
576 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
577 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
578 *CompPltGet_1
579 '
580 'Mov PProductOnPltGet_1      '�{�̏��
581 'Wait M_In(11278) = 1        '�ʒu���ߏo�[���o(�R�����g�A�E�g11/4����)
582 MRtn = frInCheck(11278,1,MSETTIMEOUT05&)        '�ʒu���ߏo�[���o
583 If MRtn = 1 Then GoTo *CompPltGet_2
584 fErrorProcess(11,231,282,0)
585 If M_20# = MNext% Then M_20# = MClear%
586 If M_20# = MAbout% Or M_20# = MNgProcess% Then
587     Mov PProductOnPltGet_2
588     Mov PInitialPosition
589     M_Out(12264) = 0            '�ʒu���ߏoOFF
590     M_Out(12265) = 1            '�ʒu���ߖ�ON
591     Break
592 EndIf
593 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
594 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
595 If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
596 *CompPltGet_2
597 Mov PProductOnPltGet_1      '�{�̏��
598 '
599 M_Out(12264) = 0            '�ʒu���ߏoOFF
600 M_Out(12265) = 1            '�ʒu���ߖ�ON
601 Ovrd 25
602 Mvs PProductOnPltGet        '�{�̂����ʒu
603 '
604 *RE_PLT_GET_2
605 '
606 If M_20# = MContinue% Then M_20# = MClear%
607 '
608 MRtn = frInCheck(11279,1,MSETTIMEOUT05&)    '�ʒu���ߖߒ[���o
609 If MRtn = 1 Then GoTo *CompPushIni
610 fErrorProcess(11,234,284,0)
611 If M_20# = MNext% Then M_20# = MClear%
612 If M_20# = MAbout% Or M_20# = MNgProcess% Then
613     Mvs PProductOnPltGet_1
614     Mov PProductOnPltGet_2
615     Mov PInitialPosition
616 EndIf
617 If M_20# = MAbout% Or M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
618 If M_20# = MContinue% Then
619     M_Out(12265) = 0            '�ʒu���ߖ�OFF
620     M_Out(12264) = 1            '�ʒu���ߏoON
621     Dly 1.0
622     M_Out(12264) = 0                            '�ʒu���ߏoOFF
623     M_Out(12265) = 1                            '�ʒu���ߖ�ON
624 EndIf
625 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
626 *CompPushIni
627 '
628 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
629 M_Out(12256) = 1            '�{�̃`���b�N��ON
630 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '�{�̃`���b�N�Z���T�[ON
631 If MRtn = 1 Then GoTo *CompPltGet_3
632 M_Out(12256) = 0            '�{�̃`���b�N��OFF
633 M_Out(12257) = 1            '�{�̃`���b�N�JON
634 Dly 2.0
635 Mvs PProductOnPltGet_1
636 Mov PProductOnPltGet_2
637 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
638 M_Out(12256) = 1            '�{�̃`���b�N��ON
639 fErrorProcess(11,244,284,0)
640 If M_20# = MNext% Then M_20# = MClear%
641 If M_20# = MAbout% Or M_20# = MNgProcess% Then
642     Mov PInitialPosition
643     Break
644 EndIf
645 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
646 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
647 M_Out(12256) = 0            '�{�̃`���b�N��OFF
648 M_Out(12257) = 1            '�{�̃`���b�N�JON
649 Dly 2.0
650 Mov PProductOnPltGet_1
651 Mvs PProductOnPltGet
652 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
653 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
654 M_Out(12256) = 1            '�{�̃`���b�N��ON
655 Dly 2.0
656 *CompPltGet_3
657 '
658 MRth = frInCheck(11264,1,MSETTIMEOUT05&)        '�{�̌��o�Z���T�[ON
659 If MRtn = 1 Then GoTo *CompPltGet_4
660 M_Out(12256) = 0            '�{�̃`���b�N��OFF
661 M_Out(12257) = 1            '�{�̃`���b�N�JON
662 Dly 2.0
663 Mvs PProductOnPltGet_1
664 Mov PProductOnPltGet_2
665 fErrorProcess(11,252,284,0)
666 If M_20# = MNext% Then M_20# = MClear%
667 If M_20# = MAbout% Or M_20# = MNgProcess% Then
668     Mov PInitialPosition
669     Break
670 EndIf
671 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
672 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
673 Mov PProductOnPltGet_1
674 Mvs PProductOnPltGet
675 If M_20# = MContinue% Then GoTo *RE_PLT_GET_2
676 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
677 M_Out(12256) = 1            '�{�̃`���b�N��ON
678 Dly 2.0
679 *CompPltGet_4
680     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
681 Mvs PProductOnPltGet_1      '�{�̏��
682     MRtn = FnCtlValue2(99)       '�Ǐ��J�n�M��OFF  2022/04/28 �n��
683 '
684 'Ovrd 100
685 Accel 50 , 50
686 MOverride = 2000 / M_OPovrd
687 If MOverride >100 Then MOverride = 100
688 Ovrd MOverride
689 '
690 Mov PProductOnPltGet_2      '�{�̉��_
691 '
692 '���i���˂����{1�ɒu��
693 Mov PProductOnRoboSet_2     '�˂����{1���_
694 'Wait M_In(11888) = 1        '�˂����{1��~1��M
695 MScrewRoboNgFlg% = 0
696 Accel 100 , 100
697 MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
698 If MRtn = 0 Then MScrewRoboNgFlg% = 1
699 If MScrewRoboNgFlg% = 1 Then GoTo *ProductOnPltSet
700 Mov PProductOnRoboSet_1     '�˂����{1���
701 Ovrd 10
702 Mvs PProductOnRoboSet       '�{�̒u���ʒu
703 Dly 0.2
704 *RE_ROBO_SET
705 If M_20# = MContinue% Then M_20# = MClear%
706 '
707 Dly 0.3
708 M_Out(12256) = 0            '�{�̃`���b�N��OFF
709 M_Out(12257) = 1            '�{�̃`���b�N�JON
710 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)             '�{�̃`���b�N�J�Z���T�[ON
711 If MRtn = 1 Then GoTo *CompRoboSet
712 fErrorProcess(11,244,284,0)
713 If M_20# = MNext% Then M_20# = MClear%
714 If M_20# = MAbout% Or M_20# = MNgProcess% Then
715     MScrewRoboNgFlg% = 1
716     Mvs PProductOnRoboSet_1
717     Mov PProductOnRoboSet_2
718     Break
719 EndIf
720 If M_20# = MAbout% Then GoTo *ProductOnPltSet    '�O�̂��ߐ��i��u���ɍs��������s��
721 If M_20# = MNgProcess% Then GoTo *ProductOnPltSet    '�O�̂��ߐ��i��u���ɍs��������s��
722 If M_20# = MContinue% Then GoTo *RE_ROBO_SET
723 *CompRoboSet
724 '
725 Mvs PProductOnRoboSet_1     '�˂����{1���
726 Ovrd 100
727 Mvs PProductOnRoboSet_2     '�˂����{1���_
728 '
729 '����L���p���b�g������
730 Mov PPlateLGet_2            '����L�����_
731 M_Out(12866) = 1 Dly 0.5    '�˂����{1����ĊJ(��~1�`��~2)
732 *RE_PLATE_L_GET_1
733 If M_20# = MContinue% Then M_20# = MClear%
734 '
735 M_Out(12257) = 0            '�{�̃`���b�N�JOFF(�ȉ�3�s,���󂯎�莞�ז��ɂȂ邽��)
736 M_Out(12256) = 1            '�{�̃`���b�N��ON
737 '
738 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '�{�̃`���b�N�Z���T�[ON
739 If MRtn = 1 Then GoTo *CompPlateLGet_1
740 fErrorProcess(11,245,284,0)
741 If M_20# = MNext% Then M_20# = MClear%
742 If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
743 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
744 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
745 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_1
746 *CompPlateLGet_1
747 '
748 Mov PPlateLGet_1            '����L�����
749 Ovrd 25
750 M_Out(12259) = 0            '����L�V�����_�[��OFF
751 M_Out(12258) = 1            '����L�V�����_�[�oON
752 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)        '����L�V�����_�[�o�[���o�Z���T�[ON
753 If MRtn = 1 Then GoTo *CompPlateLGet_2
754 fErrorProcess(11,246,284,0)
755 If M_20# = MNext% Then M_20# = MClear%
756 If M_20# = MAbout% Or M_20# = MNgProcess% Then
757     Mov PPlateLGet_2
758     Mov PInitialPosition
759 EndIf
760 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
761 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
762 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_1
763 *CompPlateLGet_2
764 '
765 *RE_PLATE_L_GET_2
766 If M_20# = MContinue% Then M_20# = MClear%
767 '
768 M_Out(12262) = 0            '���`���b�N��OFF
769 M_Out(12263) = 1            '���`���b�N�JON
770 Fine 0.05 , P               '����0.05[mm]�ȓ�
771 Mvs PPlateLGet              '����L�����ʒu
772 Fine 0 , P                  'Fine����
773 M_Out(12263) = 0            '���`���b�N�JOFF
774 M_Out(12262) = 1            '���`���b�N��ON
775 'Wait M_In(11271) = 1        '���`���b�N���o�Z���T�[ON
776 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)        '����L�`���b�N���o�Z���T�[ON
777 Dly 0.5
778 Ovrd 10                     '�I�[�o�[���C�h�ύX(22/12/09����)
779 Accel 10,10                 '�����x�ύX(22/12/09����)
780 Mvs PPlateLGet_1            '����L�����
781 If MRtn = 1 Then GoTo *CompPlateLGet_3
782 fErrorProcess(11,250,292,0) '284��292�ɕύX(6/7����)
783 If M_20# = MNext% Then M_20# = MClear%
784 If M_20# = MAbout% Or M_20# = MNgProcess% Then
785     Mov PPlateLGet_2
786     Mov PInitialPosition
787 EndIf
788 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
789 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
790 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
791 *CompPlateLGet_3
792 '
793 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         '����L���o
794 If MRtn = 1 Then GoTo *CompPlateLGet_4
795 fErrorProcess(11,254,292,0) '284��292�ɕύX(6/7����)
796 If M_20# = MNext% Then M_20# = MClear%
797 If M_20# = MAbout% Or M_20# = MNgProcess% Then
798     Mov PPlateLGet_2
799     Mov PInitialPosition
800 EndIf
801 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
802 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
803 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
804 *CompPlateLGet_4
805 'Dly 5                      'test(�b��R�����g�A�E�g)
806 'M_Out(12256) = 0            '�{�̃`���b�N��OFF(���󂯎�莞�ז��ɂȂ邽��)�ȉ�3�s�b��폜(11/17����)
807 'M_Out(12257) = 1            '�{�̃`���b�N�JON(���󂯎�莞�ז��ɂȂ邽��)
808 'MRtn = frInCheck(11265,1,MSETTIMEOUT05&)         '�{�̃`���b�N�J�Z���T�[ON
809 'If MRtn = 0 Then
810 '    fErrorProcess()         '�G���[����
811 'EndIf
812 Ovrd 50             '�I�[�o�[���C�h�ύX(22/12/09����)
813 Mov PPlateLGet_2            '����L�����_
814 '
815 '����L��u��
816 Mov PPlateLSet_2            '����L�u�����_
817 Accel 100,100       '�����x�ύX(22/12/09����)
818 Ovrd 100            '�I�[�o�[���C�h�ύX(22/12/09����)
819 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)         '������x����L���o
820 If MRtn = 1 Then GoTo *CompPlateLGet_5
821 fErrorProcess(11,254,292,0) '284��292�ɕύX(6/7����)
822 If M_20# = MNext% Then M_20# = MClear%
823 If M_20# = MAbout% Or M_20# = MNgProcess% Then
824     Mov PPlateLGet_2
825     Mov PInitialPosition
826 EndIf
827 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
828 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
829 If M_20# = MContinue% Then
830     Mov PPlateLGet_2
831     Mov PPlateLGet_1
832 EndIf
833 If M_20# = MContinue% Then GoTo *RE_PLATE_L_GET_2
834 *CompPlateLGet_5
835 'Wait M_In(11889) = 1        '�˂����{1��~2��M
836 MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
837 If MRtn = 0 Then Mov PInitialPosition
838 If MRtn = 0 Then GoTo *ASSY_ERROR_END
839 '
840 Fine 0.05 , P
841 '
842 Mov PPlateLSet_1            '����L�u�����
843 Ovrd 10
844 Mvs PPlateLSet              '����L��u���ʒu
845 '
846 Fine 0 , P
847 '
848 M_Out(12866) = 1 Dly 0.5    '�˂����{1����ĊJ(��~2�`��~3)
849 'Wait M_In(11890) = 1        '�˂����{1��~3��M
850 MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����
851 'If MRtn = 0 Then
852 '    Mvs PPlateLSet_1
853 '    Mov PPlateLSet_2
854 '    Mov PInitialPosition
855 'EndIf
856 If MRtn = 0 Then GoTo *ASSY_ERROR_END
857 M_Out(12262) = 0            '���`���b�N��OFF
858 M_Out(12263) = 1            '���`���b�N�JON
859 Dly 0.5
860 Mvs PPlateLSet_1            '����L�u�����
861 Ovrd 100
862 M_Out(12258) = 0            '����L�V�����_�[�oOFF
863 M_Out(12259) = 1            '����L�V�����_�[��ON
864 Mov PPlateLSet_2            '����L�u�����_
865 '
866     ' ���i�����v�����M'12/20�ʒu�ύX(����)
867     M_Out(12787) = 1
868 '
869 *RE_CYLINDER_L_INI
870 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)        'L���V�����_�[�ߌ��o
871 If MRtn = 1 Then GoTo *CompCylinderLIni
872 fErrorProcess(11,247,284,0)
873 If M_20# = MNext% Then M_20# = MClear%
874 If M_20# = MAbout% Or M_20# = MNgProcess% Then
875     Mov PPlateLSet_3
876     Mov PInitialPosition
877     Break
878 EndIf
879 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
880 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
881 If M_20# = MContinue% Then
882     M_Out(12258) = 0            '����L�V�����_�[�oOFF
883     M_Out(12259) = 1            '����L�V�����_�[��ON
884 EndIf
885 If M_20# = MContinue% Then GoTo *RE_CYLINDER_L_INI
886 *CompCylinderLIni
887 '����L�u���ʒu�摜����
888 Mov PPlateLCheck_2          '����L�����ʉߓ_
889 Mvs PPlateLCheck            '����L�����ʒu
890 *RE_L_CHECK
891 PInspPosition(1) = PPlateLCheck
892 MInspGroup%(1) = 2
893 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
894 If MRtn = 1 Then GoTo *CompLCheck
895 fErrorProcess(11,43,3,0)
896 If M_20# = MNext% Then M_20# = MClear%
897 If M_20# = MAbout% Or M_20# = MNgProcess% Then
898     Mov PInitialPosition
899     Break
900 EndIf
901 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
902 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
903 If M_20# = MContinue% Then GoTo *RE_L_CHECK
904 *CompLCheck
905 '
906 M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~3�`��~4)
907 '
908 '����R�������@������
909 '
910 ''    ' ���i�����v�����M'12/20�ʒu�ύX(����)
911 '    M_Out(12787) = 1
912     '    ' ���i���������҂�'12/21�ʒu�ύX(����)
913 '    Wait M_In(11810) = 1
914 '
915 'Mov PPlateRGet_4            '�o�H1
916 Mov PPlateRGet_3            '�o�H2
917 Mov PPlateRGet_2            '����R�����_
918     '    ' ���i���������҂�(�����ύX2/27����)
919 *RE_FEEDER_READY
920     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/27 �n��
921 '    Wait M_In(11810) = 1
922 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
923 If MRtn = 1 Then GoTo *CompFeederReady
924 '   ' ���i�����v���I��
925 M_Out(12787) = 0
926 fErrorProcess(11,289,290,0) '284��290�ɕύX(6/7����)
927 If M_20# = MNext% Then M_20# = MClear%
928 If M_20# = MAbout% Or M_20# = MNgProcess% Then
929     Mov PBracketRGet_2
930     Mov PBracketRGet_3
931     Mov PBracketRSet_3
932     Mov PInitialPosition1
933 EndIf
934 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
935 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
936     ' ���i�����v��
937 M_Out(12787) = 1
938 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
939 *CompFeederReady
940 '    ' ���i�����v���I��
941     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/27 �n��
942     M_Out(12787) = 0
943 '
944 *RE_PLATE_R_GET_1
945 '
946 If M_20# = MContinue% Then M_20# = MClear%
947 '
948 M_Out(12257) = 0            '�{�̃`���b�N�JOFF(�ȉ�3�s,���󂯎�莞�ז��ɂȂ邽��)
949 M_Out(12256) = 1            '�{�̃`���b�N��ON
950 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '�{�̃`���b�N�Z���T�[ON
951 If MRtn = 1 Then GoTo *CompPlateRGet_1
952 fErrorProcess(11,245,284,0)
953 If M_20# = MNext% Then M_20# = MClear%
954 If M_20# = MAbout% Or M_20# = MNgProcess% Then
955     Mov PPlateRGet_2
956     Mov PPlateRGet_3
957 '    Mov PPlateRGet_4
958     Mov PInitialPosition
959     Break
960 EndIf
961 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
962 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
963 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
964 *CompPlateRGet_1
965 '
966 *RE_PLATE_R_GET_2
967 If M_20# = MContinue% Then M_20# = MClear%
968 '
969 Mov PPlateRGet_1            '����R�����
970 Ovrd 25
971 M_Out(12261) = 0            '����R�V�����_�[��OFF
972 M_Out(12260) = 1            '����R�V�����_�[�oON
973 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)        '����R�V�����_�[�o�[���o�Z���T�[ON
974 If MRtn = 1 Then GoTo *CompPlateRGet_2
975 fErrorProcess(11,248,284,0)
976 If M_20# = MNext% Then M_20# = MClear%
977 If M_20# = MAbout% Or M_20# = MNgProcess% Then
978     Mov PPlateRGet_2
979     Mov PPlateRGet_3
980 '    Mov PPlateRGet_4
981     Mov PInitialPosition
982     Break
983 EndIf
984 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
985 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
986 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
987 *CompPlateRGet_2
988 '
989 M_Out(12262) = 0            '���`���b�N��OFF
990 M_Out(12263) = 1            '���`���b�N�JON
991 Fine 0.05 , P               '����0.05[mm]�ȓ�
992 Mvs PPlateRGet              '����R�����ʒu
993 Fine 0 , P                  'Fine����
994 M_Out(12263) = 0            '���`���b�N�JOFF
995 M_Out(12262) = 1            '���`���b�N��ON
996 MRtn = frInCheck(11276,1,MSETTIMEOUT05&)        '����R�`���b�N���o�Z���T�[ON
997 Dly 0.5
998 Ovrd 10         '�I�[�o�[���C�h�ύX(22/12/09����)
999 Accel 20,20     '�����x�ύX(22/12/09����)
1000 Mvs PPlateRGet_1            '����L�����
1001 If MRtn = 1 Then GoTo *CompPlateRGet_3
1002 fErrorProcess(11,250,292,0) '284��292�ɕύX(6/7����)
1003 If M_20# = MNext% Then M_20# = MClear%
1004 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1005     Mov PPlateLGet_2
1006     Mov PInitialPosition
1007 EndIf
1008 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1009 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1010 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
1011 *CompPlateRGet_3
1012 '
1013 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '����R���o
1014 If MRtn = 1 Then GoTo *CompPlateRGet_4
1015 fErrorProcess(11,254,292,0) '284��292�ɕύX(6/7����)
1016 If M_20# = MNext% Then M_20# = MClear%
1017 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1018     Mov PPlateRGet_2
1019     Mov PPlateRGet_3
1020 '    Mov PPlateRGet_4
1021     Mov PInitialPosition
1022     Break
1023 EndIf
1024 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1025 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1026 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_2
1027 *CompPlateRGet_4
1028 '
1029 M_Out(12256) = 0            '�{�̃`���b�N��OFF(���󂯎�莞�ז��ɂȂ邽��)
1030 M_Out(12257) = 1            '�{�̃`���b�N�JON(���󂯎�莞�ז��ɂȂ邽��)
1031 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '�{�̃`���b�N�J�Z���T�[ON
1032 If MRtn = 1 Then GoTo *CompPlateRGet_5
1033 fErrorProcess(11,244,284,0)
1034 If M_20# = MNext% Then M_20# = MClear%
1035 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1036     Mov PPlateRGet_2
1037     Mov PPlateRGet_3
1038 '    Mov PPlateRGet_4
1039     Mov PInitialPosition
1040     Break
1041 EndIf
1042 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1043 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1044 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
1045 *CompPlateRGet_5
1046 '
1047 'Ovrd 100                   '�R�����g�A�E�g2/15����
1048 Accel 50 , 50               '�����x�ύX(22/12/09����)
1049 Mov PPlateRGet_2            '����R�����_
1050 Mov PPlateRGet_3            '�o�H2
1051 'Accel 100 , 100
1052 'Ovrd 100
1053 ''    ' ���i�����v���I��(�����ʒu�ύX2/11����)
1054 '    M_Out(12787) = 0
1055 ''    ' ���i�擾�������M(�p���X)
1056 '    M_Out(12800) = 1 Dly 0.5
1057 '    '
1058 'Mov PPlateRGet_4            '�o�H1
1059 '
1060 '����R��u��
1061 Mov PPlateRSet_3            '�o�H
1062 Mov PPlateRSet_2            '����R�u�����_
1063 Accel 100 , 100             '�����x�ύX(22/12/09����)
1064 Ovrd 100                    '�I�[�o�[���C�h�ύX(22/12/09����)
1065 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '������x����R���o
1066 If MRtn = 1 Then GoTo *CompRGet
1067 fErrorProcess(11,254,292,0) '284��292�ɕύX(6/7����)
1068 If M_20# = MNext% Then M_20# = MClear%
1069 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1070     Mov PPlateRSet_3
1071     Mov PPlateRGet_3
1072 '    Mov PPlateRGet_4
1073     Mov PInitialPosition
1074     Break
1075 EndIf
1076 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1077 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1078 If M_20# = MContinue% Then
1079     Mov PPlateRSet_3
1080     Mov PPlateRGet_3
1081     Mov PPlateRGet_2
1082     Mov PPlateRGet_1
1083 EndIf
1084 If M_20# = MContinue% Then GoTo *RE_PLATE_R_GET_1
1085 *CompRGet
1086 '    ' ���i�����v���I��
1087     M_Out(12787) = 0
1088 '    ' ���i�擾�������M(�p���X)
1089     M_Out(12800) = 1 Dly 0.5
1090     '
1091 'Wait M_In(11891) = 1        '�˂����{1��~4��M
1092 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
1093 If MRtn = 0 Then Mov PInitialPosition
1094 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1095 '
1096 Fine 0.05 , P
1097 '
1098 Mov PPlateRSet_1            '����R�u�����
1099 Ovrd 10
1100 Mvs PPlateRSet              '����R��u���ʒu
1101 '
1102 Fine 0 , P
1103 '
1104 M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~4�`��~5)
1105 'Wait M_In(11892) = 1        '�˂����{1��~5��M
1106 MRtn = fScrewTighenRoboCheck(11892)    '��~��Ԃ���M����
1107 'If MRtn = 0 Then
1108 '    Mvs PPlateRSet_1
1109 '    Mov PPlateRSet_2
1110 '    Mov PInitialPosition
1111 'EndIf
1112 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1113 M_Out(12262) = 0            '���`���b�N��OFF
1114 M_Out(12263) = 1            '���`���b�N�JON
1115 Dly 0.5
1116 Mvs PPlateRSet_1            '����L�u�����
1117 Ovrd 100
1118 M_Out(12260) = 0            '����R�V�����_�[�oOFF
1119 M_Out(12261) = 1            '����R�V�����_�[��ON
1120 Mov PPlateRSet_2            '����R�u�����_
1121 '
1122 '
1123 ''����R�u���ʒu�摜����
1124 Mov PPlateRCheck_2          '����R�����ʉߓ_
1125 Mvs PPlateRCheck            '����R�����ʒu
1126 *RE_R_CHECK
1127 If M_20# = MContinue% Then M_20# = MClear%
1128 PInspPosition(1) = PPlateRCheck
1129 MInspGroup%(1) = 3
1130 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
1131 If MRtn = 1 Then GoTo *CompCheckR
1132 fErrorProcess(11,43,3,0)
1133 If M_20# = MNext% Then M_20# = MClear%
1134 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1135     Mvs PPlateRCheck_2
1136     Mov PInitialPosition
1137 EndIf
1138 If M_20# = MAbout% Or M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1139 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1140 *CompCheckR
1141 'If MRtn <> 1 Then
1142 '   '�G���[����
1143 'EndIf
1144 M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~5�`��~6)
1145 '
1146 '�˂����{1�̐��i�����
1147 Mov PProductOnRoboGet_2     '�˂����{1���_
1148 '
1149 *RE_CYLINDER_R_INI
1150 MRtn = frInCheck(11273,1,MSETTIMEOUT05&)        'R���V�����_�[�ߌ��o
1151 If MRtn = 1 Then GoTo *CompCylinderRIni
1152 fErrorProcess(11,249,284,0)
1153 If M_20# = MNext% Then M_20# = MClear%
1154 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1155     Mov PPlateRSet_3
1156     Mov PPlateRGet_3
1157 '    Mov PPlateRGet_4
1158     Mov PInitialPosition
1159     Break
1160 EndIf
1161 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1162 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1163 If M_20# = MContinue% Then
1164     M_Out(12260) = 0            '����R�V�����_�[�oOFF
1165     M_Out(12261) = 1            '����R�V�����_�[��ON
1166 EndIf
1167 If M_20# = MContinue% Then GoTo *RE_CYLINDER_R_INI
1168 *CompCylinderRIni
1169 '
1170 'Wait M_In(11893) = 1        '�˂����{1��~6��M
1171 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   '�˂����{1��������M
1172 MRtn = fScrewTighenRoboCheck(11893)    '��~��Ԃ���M����
1173 If MRtn = 0 Then Mov PInitialPosition
1174 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1175 'Mvs PProductOnRoboGet_1     '�˂����{1���(2022/1/11�ړ��^�C�~���O�ύX(����))
1176 'Ovrd 25
1177 '
1178 *RE_ROBO_GET
1179 '
1180 If M_20# = MContinue% Then M_20# = MClear%
1181 '
1182 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1183 M_Out(12257) = 1            '�{�̃`���b�N�JON
1184 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)        '�{�̃`���b�N�J�Z���T�[ON
1185 If MRtn = 1 Then GoTo *CompRoboGet_1
1186 Mov PInitialPosition
1187 fErrorProcess(11,244,284,0)
1188 If M_20# = MNext% Then M_20# = MClear%
1189 'If M_20# = MAbout% Or M_20# = MNgProcess% Then
1190 '    Mvs PProductOnRoboGet_2
1191 '    Mov PInitialPosition
1192 '    Break
1193 'EndIf
1194 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1195 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1196 Mov PProductOnRoboGet_2
1197 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1198 *CompRoboGet_1
1199 '
1200 Mvs PProductOnRoboGet_1     '�˂����{1���(2022/1/11�ړ��^�C�~���O�ύX(����))
1201 Ovrd 25
1202 '
1203 Mvs PProductOnRoboGet       '�{�̂����ʒu
1204 'M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~6�`����)�ʒu�ύX(12/24����)
1205 'Wait M_In(11876) = 1        '�˂����{1��������M
1206 'MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1207 'If MRtn = 0 Then
1208 '    Mvs PProductOnRoboGet_1
1209 '    Mov PProductOnRoboGet_2
1210 '    Mov PInitialPosition
1211 'EndIf
1212 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
1213 '
1214 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1215 M_Out(12256) = 1            '�{�̃`���b�N��ON
1216 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)        '�{�̃`���b�N�Z���T�[ON
1217 If MRtn = 1 Then GoTo *CompRoboGet_2
1218 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1219 M_Out(12257) = 1            '�{�̃`���b�N�JON
1220 Dly 2.0
1221 Mvs PProductOnRoboGet_1
1222 Mvs PProductOnRoboGet_2
1223 Mov PInitialPosition
1224 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1225 M_Out(12256) = 1            '�{�̃`���b�N��ON
1226 fErrorProcess(11,245,284,0)
1227 If M_20# = MNext% Then M_20# = MClear%
1228 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1229 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1230 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1231 M_Out(12257) = 1            '�{�̃`���b�N�JON
1232 Dly 2.0
1233 Mov PProductOnRoboGet_2
1234 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1235 Mvs PProductOnRoboGet_1
1236 Mvs PProductOnRoboGet
1237 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1238 M_Out(12256) = 1            '�{�̃`���b�N��ON
1239 Dly 2.0
1240 *CompRoboGet_2
1241 '
1242 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)        '�{�̌��o�Z���T�[ON
1243 'Mvs PProductOnRoboGet_1     '�˂����{1���
1244 If MRtn = 1 Then GoTo *CompRoboGet_3
1245 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1246 M_Out(12257) = 1            '�{�̃`���b�N�JON
1247 Dly 2.0
1248 Mvs PProductOnRoboGet_1
1249 Mvs PProductOnRoboGet_2
1250 Mov PInitialPosition
1251 fErrorProcess(11,252,284,0)
1252 If M_20# = MNext% Then M_20# = MClear%
1253 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1254 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1255 Mov PProductOnRoboGet_2
1256 If M_20# = MContinue% Then GoTo *RE_ROBO_GET
1257 Mvs PProductOnRoboGet_1
1258 Mvs PProductOnRoboGet
1259 M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1260 M_Out(12256) = 1            '�{�̃`���b�N��ON
1261 Dly 2.0
1262 *CompRoboGet_3
1263 '
1264 M_Out(12866) = 1 Dly 0.5    '�˂����{����ĊJ(��~6�`����)
1265 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1266 'Dly 5'�f�o�b�O
1267 If MRtn = 0 Then
1268     M_Out(12256) = 0
1269     M_Out(12257) = 1
1270     Wait M_In(11265) = 1
1271     Mvs PProductOnRoboGet_1
1272     Mvs PProductOnRoboGet_2
1273     Mov PInitialPosition
1274 EndIf
1275 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1276 '
1277 Mvs PProductOnRoboGet_1     '�˂����{1���
1278 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)        '�{�̌��o�Z���T�[ON
1279 If MRtn = 1 Then GoTo *CompRoboGet_4
1280 MOverride = 2000 / M_OPovrd
1281 If MOverride >100 Then MOverride = 100
1282 Ovrd MOverride
1283 Mov PProductOnRoboGet_2
1284 Mov PInitialPosition
1285 fErrorProcess(11,252,284,0)             'MContinue%��MClear%
1286 If M_20# = MNext% Then M_20# = MClear%
1287 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1288 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1289 If M_20# = MContinue% Then M_20# = MClear%
1290 GoTo *ProductOnPltSet
1291 *CompRoboGet_4
1292 '
1293 'Ovrd 100
1294 Accel 50 , 50
1295 MOverride = 2000 / M_OPovrd
1296 If MOverride >100 Then MOverride = 100
1297 Ovrd MOverride
1298 'Mvs PProductOnRoboGet_1     '�˂����{1���
1299 Mov PProductOnRoboGet_2     '�˂����{1���_
1300 '
1301 *ProductOnPltSet
1302 '�p���b�g�ɐ��i��u��
1303 Mov PProductOnPltSet_2     '�p���b�g���_
1304 Accel 100 , 100
1305 Mov PProductOnPltSet_1     '�p���b�g���
1306 Ovrd 10
1307 Mvs PProductOnPltSet       '�{�̒u���ʒu
1308 Dly 0.2
1309 '
1310 *RE_PLT_SET
1311 '
1312 If MScrewRoboNgFlg% = 1 And M_20# = MContinue% Then M_20# = MRtn
1313 If M_20# = MContinue% Then M_20# = MClear%
1314 '
1315 M_Out(12256) = 0            '�{�̃`���b�N��OFF
1316 M_Out(12257) = 1            '�{�̃`���b�N�JON
1317 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    '�{�̃`���b�N�J�Z���T�[ON
1318 If MRtn = 1 Then GoTo *CompPltSet_1
1319 If MScrewRoboNgFlg% = 1 Then
1320     MRtn = M_20#
1321     M_20# = MClear%
1322 EndIf
1323 fErrorProcess(11,244,284,0)
1324 If M_20# = MNext% Then M_20# = MClear%
1325 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1326 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1327 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1328 *CompPltSet_1
1329 If MScrewRoboNgFlg% = 1 Then M_20# = MRtn
1330 '
1331 Mvs PProductOnPltSet_1     '�p���b�g���
1332 Ovrd 100
1333 Mvs PProductOnPltSet_2     '�p���b�g���_
1334 '
1335 'Mov PInitialPosition        '�b��폜(11/17����)
1336 '
1337     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1338 Mov PTicketRead_1
1339     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1340 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
1341 M_Out(12868) = 1 Dly 0.5    '�˂����{1�˂����ߊ����𑗐M
1342 '
1343 M_20# = MAssyOK%            'Assy����I��
1344 '
1345 GoTo *AssyEnd
1346 '
1347 *ASSY_ERROR_END
1348     M_Out(12264) = 0            '�ʒu���ߏoOFF
1349     M_Out(12265) = 1            '�ʒu���ߖ�ON
1350 *AssyEnd
1351 *fnAssyStart_FEndPosi
1352     Exit Function
1353 FEnd
1354 '
1355 '��fnPiasCheck
1356 ''' <summary>
1357 ''' PIAS�`�P�b�g�Ǎ���
1358 ''' </summary>
1359 ''' <returns>   0 : NG
1360 '''             1 : OK(�Ǎ��݊���)
1361 ''' </returns>
1362 ''' <remarks>
1363 ''' Date   : 2021/07/07 : M.Hayakawa
1364 ''' </remarks>'
1365 Function M% fnPiasCheck
1366     fnPiasCheck = 0
1367     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1368     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1369 '
1370 *RETRY_PIAS
1371     M_20# = MClear%
1372     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1373     '
1374     '�yID�`�P�b�g�ǂݍ��݁z
1375     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1376     MInspGroup%(1) = 1              '����G�ԍ�
1377     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1378 '
1379     '�G���[�̏ꍇ
1380     If MRtn <> 1 Then
1381         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1382         If MRtn <> 1 Then
1383             'D720 -> D1300 �R�s�[�v��
1384             M_Out(12565) = 1
1385             Dly 0.5
1386             M_Out(12565) = 0
1387             '�G���[�����L�q
1388             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1389             'GOT KEY���͑҂�
1390             MKeyNumber = fnKEY_WAIT()
1391             '
1392             Select MKeyNumber
1393                 Case MNext%         '���ւ�I�������ꍇ
1394                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1395                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1396                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1397                     Break
1398                 Case MAbout%        '��~��I�������ꍇ
1399                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1400                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1401                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1402                     Break
1403                 Case MNgProcess%    'NG��I�������ꍇ
1404                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1405                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1406                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1407                     Break
1408                 Case MContinue%     '�p����I�������ꍇ
1409                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1410                     M_20# = MContinue%
1411                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1412                     Break
1413             End Select
1414         EndIf
1415     EndIf
1416 '----------D720 -> D1300 �R�s�[�v��----------
1417     M_Out(12565) = 1
1418     Dly 0.5
1419     M_Out(12565) = 0
1420 '----------�ʐM�m�F������----------
1421     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1422     MRtn = 0                ' ������
1423     M_20# = MClear%         ' ������
1424     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1425     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1426     If MRtn <> 1 Then
1427         If M_20# = MContinue% Then
1428             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1429         Else
1430             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1431         EndIf
1432     EndIf
1433 '----------�H�������m�F----------
1434     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1435     MRtn = 0                ' ������
1436     M_20# = MClear%         ' ������
1437     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1438     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1439     If MRtn <> 1 Then
1440         If M_20# = MContinue% Then
1441             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1442         Else
1443             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1444         EndIf
1445     EndIf
1446     '
1447     fnPiasCheck = 1
1448     *fnPiasCheck_End
1449     Exit Function
1450 FEnd
1451 '
1452 '��fnPCComuCheck
1453 ''' <summary>
1454 ''' PC-PLC�ʐM�`�F�b�N
1455 ''' </summary>
1456 ''' <returns>   0 : NG
1457 '''             1 : OK(�Ǎ��݊���)
1458 ''' </returns>
1459 ''' <remarks>
1460 ''' Date   : 2021/07/07 : M.Hayakawa
1461 ''' </remarks>'
1462 Function M% fnPCComuCheck
1463     fnPCComuCheck = 0
1464     MJudge% = 0                                  '������
1465     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1466     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1467     '
1468     For MStaNo = 0 To 5
1469         '
1470         If M_In(MIN_PIAS_ComOK%) = 1 Then
1471             'PC�ʐMOK(M400)
1472             MJudge% = MOK%
1473             MStaNo = 5
1474             Break
1475         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1476             'toRBT_�ʐM�m�Ftime out
1477             MJudge% = MNG%
1478             MCommentD1001 = 15
1479             MCommentD1002 = 21
1480             MStaNo = 5
1481             Break
1482         Else
1483             'toRBT_�ʐM�m�Ftime out
1484             MJudge% = MNG%
1485             MCommentD1001 = 14
1486             MCommentD1002 = 21
1487             Break
1488         EndIf
1489     Next MStaNo
1490     '
1491     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1492     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1493     '
1494     '�G���[���
1495     If MJudge% <> MOK% Then
1496         M_20# = MClear%     '������
1497         '�G���[�����L�q
1498         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1499         'GOT KEY���͑҂�
1500         MKeyNumber = fnKEY_WAIT()
1501         '
1502         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1503             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1504             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1505             Break
1506         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1507             M_20# = MPass%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1508             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1509             Break
1510         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1511             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1512             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1513             Break
1514         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1515             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1516             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1517             Break
1518         EndIf
1519     Else
1520         'OK�̏ꍇ
1521         fnPCComuCheck = 1
1522     EndIf
1523     Exit Function
1524 FEnd
1525 '
1526 '��fnProcessCheck
1527 ''' <summary>
1528 ''' �H�������m�F
1529 ''' </summary>
1530 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1531 '''             -1�F�O�H������NG  -2�F���H����������
1532 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1533 '''             -5�F���������G���[
1534 ''' </returns>
1535 ''' <remarks>
1536 ''' Date   : 2021/07/07 : M.Hayakawa
1537 ''' </remarks>'
1538 Function M% fnProcessCheck
1539     fnProcessCheck = 0
1540     MJudge% = MNG%      '��UNG���������Ƃ���
1541 '----------�H�������m�F----------
1542     MCommentD1001 = 0   '�R�����g������
1543     For MStaNo = 0 To 5
1544         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1545         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1546         '
1547         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1548             MJudge% = MOK%
1549             fnAutoScreenComment(85)     ' AUTO���
1550             MStaNo = 5
1551             Break
1552         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1553             MFlgLoop% = 0
1554             MJudge% = MNG%
1555             MCommentD1001 = 27
1556             MCommentD1002 = 22
1557             fnAutoScreenComment(94)     ' AUTO���
1558             fnProcessCheck = -2         ' NG��-2��Ԃ�
1559             MStaNo = 5
1560             Break
1561         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1562            MJudge% = MNG%
1563             MCommentD1001 = 31
1564             MCommentD1002 = 22
1565             fnAutoScreenComment(83)     ' AUTO���
1566             fnProcessCheck = -3         ' NG��-3��Ԃ�
1567             MStaNo = 5
1568             Break
1569         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1570             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1571             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1572             MJudge% = MNG%
1573             MCommentD1001 = 32
1574             MCommentD1002 = 22
1575             fnAutoScreenComment(84)     ' AUTO���
1576             fnProcessCheck = -1         ' NG��-1��Ԃ�
1577             Dly 1.0
1578             '�H�������m�FOFF
1579             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1580             Dly 1.0
1581            'MStaNo = 5
1582             Break
1583         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1584             MFlgLoop% = 0
1585             MJudge% = MNG%
1586             MCommentD1001 = 29
1587             MCommentD1002 = 22
1588             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1589             fnProcessCheck = -5         ' NG��-5��Ԃ�
1590             MStaNo = 5
1591             Break
1592         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1593             MJudge% = MNG%
1594             If MCommentD1001 = 32 Then
1595                 '�������Ȃ�
1596             Else
1597                 MCommentD1001 = 26
1598             EndIf
1599             MCommentD1002 = 22
1600             fnProcessCheck = -4         ' NG��-4��Ԃ�
1601             MStaNo = 5
1602             Break
1603         Else
1604             MJudge% = MNG%
1605             MCommentD1001 = 28
1606             MCommentD1002 = 22
1607         EndIf
1608     Next MStaNo
1609     '�H�������m�FOFF
1610     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1611     '�ʉߗ���NG �H�������̏ꍇ
1612     If MJudge% = MPass% Then
1613         M_20# = MPass%
1614     EndIf
1615     '
1616     '�G���[���
1617     If MJudge% <> MOK% Then
1618         M_20# = MClear%     '������
1619         '�G���[�����L�q
1620         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1621         'GOT KEY���͑҂�
1622         MKeyNumber = fnKEY_WAIT()
1623         '
1624         Select MKeyNumber
1625             Case MAbout%        '��~��I�������ꍇ
1626                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1627                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1628                 Break
1629             Case MNext%         '���ւ�I�������ꍇ
1630                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1631                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1632                 Break
1633             Case MContinue%     '�p����I�������ꍇ
1634                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1635                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1636                 Break
1637             Case MNgProcess%    'NG��I�������ꍇ
1638                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1639                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1640                 Break
1641         End Select
1642     Else
1643         fnProcessCheck = 1  ' OK��1��Ԃ�
1644     EndIf
1645     Exit Function
1646 FEnd
1647 '
1648 '��fnPiasWrite
1649 ''' <summary>
1650 ''' Pias �g�����ʏ����ݗv��
1651 ''' </summary>
1652 '''<param name="MFlg%">
1653 '''                 MOK%(1) = �H��������OK��������
1654 '''                 MNG%(0) = �H��������NG��������
1655 '''</param>
1656 '''<returns></returns>
1657 ''' <remarks>
1658 ''' Date   : 2021/07/07 : M.Hayakawa
1659 ''' </remarks>'
1660 Function M% fnPiasWrite(ByVal MFlg%)
1661       fnPiasWrite = 0
1662 *RETRY_PIASWRITE
1663     '
1664     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1665    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1666     If MFlg% = MOK% Then
1667         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1668     Else
1669         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1670     EndIf
1671     Dly 0.1                  '�O�̂���
1672     '
1673     'Pias�֏����݊J�n M305 -> ON
1674     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1675     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1676     '
1677     MJudge% = MNG%
1678     '
1679     For MStaNo = 0 To 5
1680         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1681             MJudge% = MOK%
1682             'MRet = fnAutoScreenComment(85)  'AUTO���
1683             MStaNo = 5
1684             Break
1685         '
1686         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1687             MJudge% = MNG%
1688             'MRet = fnAutoScreenComment(85)  'AUTO���
1689            MCommentD1001 = 34
1690            MCommentD1002 = 25
1691             MStaNo = 5
1692             Break
1693         '
1694         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1695             MJudge% = MNG%
1696             'MRet = fnAutoScreenComment(85)  'AUTO���
1697            MCommentD1001 = 35
1698            MCommentD1002 = 25
1699             MStaNo = 5
1700             Break
1701         '
1702         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1703             MJudge% = MNG%
1704             'MRet = fnAutoScreenComment(85)  'AUTO���
1705            MCommentD1001 = 36
1706            MCommentD1002 = 25
1707             MStaNo = 5
1708             Break
1709         '
1710         Else
1711             MJudge% = MNG%
1712            MCommentD1001 = 42
1713            MCommentD1002 = 25
1714         '
1715         EndIf
1716         '
1717     Next MStaNo
1718     '
1719     'Pias�֏����݊J�n M305 -> OfF
1720     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1721     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1722     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1723     '
1724     '
1725     '�ʉߗ���NG �H�������̏ꍇ
1726     If MJudge% = MPass% Then
1727         M_20# = MPass%
1728     EndIf
1729     '
1730    M_20# = MClear%     '������
1731     '
1732     '�G���[���
1733     If MJudge% < MOK% Then
1734     '
1735 '�c���Ă���������ł͎g�p���Ȃ����x��
1736 *RETRY_ERR_WRITE
1737         M_20# = MClear%     '������
1738         '�G���[�����L�q
1739         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1740         'GOT KEY���͑҂�
1741         MKeyNumber = fnKEY_WAIT()
1742         '
1743         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1744             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1745            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1746             Break
1747         '
1748         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1749             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1750             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1751         '
1752         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1753             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1754             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1755         '
1756         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1757             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1758            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1759             Break
1760         '
1761         EndIf
1762         '
1763         If M_20# = MClear% Then *RETRY_ERR_WRITE
1764         '
1765     EndIf
1766     '
1767     If M_20# = MContinue% Then *RETRY_PIASWRITE
1768     '
1769     fnPiasWrite = 1
1770     Exit Function
1771 FEnd
1772 '
1773 '��fnPCBNumberCheck
1774 ''' <summary>
1775 ''' Pias ��ԍ��ƍ��v��
1776 ''' </summary>
1777 '''<param name="%"></param>
1778 '''<param name="%"></param>
1779 '''<returns></returns>
1780 ''' <remarks>
1781 ''' Date   : 2021/07/07 : M.Hayakawa
1782 ''' </remarks>'
1783 Function M% fnPCBNumberCheck
1784       fnPCBNumberCheck = 0
1785     '
1786 *RETRY_PCBCHECK
1787     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1788     'Pias�֊�ƍ��J�n M310 -> ON
1789     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1790     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1791     '
1792     MJudge% = MNG%
1793     '
1794     For MStaNo = 0 To 5
1795         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1796             MJudge% = MOK%
1797             fnAutoScreenComment(96)  'AUTO���
1798             MStaNo = 5
1799             Break
1800         '
1801         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1802             MJudge% = MNG%
1803             fnAutoScreenComment(97)  'AUTO���
1804             MCommentD1001 = 37
1805             MCommentD1002 = 25
1806             MStaNo = 5
1807             Break
1808         '
1809         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1810             MJudge% = MNG%
1811             fnAutoScreenComment(98)  'AUTO���
1812             MCommentD1001 = 38
1813             MCommentD1002 = 25
1814             MStaNo = 5
1815             Break
1816         '
1817         ElseIf M_In(11580) = 1 Then                         'time out
1818             MJudge% = MNG%
1819             fnAutoScreenComment(99)  'AUTO���
1820             MCommentD1001 = 39
1821             MCommentD1002 = 25
1822             MStaNo = 5
1823             Break
1824         '
1825         Else
1826             MJudge% = MNG%
1827            MCommentD1001 = 41
1828            MCommentD1002 = 25
1829         '
1830         EndIf
1831         '
1832     Next MStaNo
1833     '
1834     'Pias�֊�ƍ��J�n M310 -> OfF
1835     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1836     '
1837     '
1838     '�ʉߗ���NG �H�������̏ꍇ
1839     If MJudge% = MPass% Then
1840         M_20# = MPass%
1841     EndIf
1842     '
1843    M_20# = MClear%     '������
1844     '
1845     '�G���[���
1846     If MJudge% < MOK% Then
1847     '
1848 '�c���Ă���������ł͎g�p���Ȃ����x��
1849 *RETRY_ERR_PCBNUMBER
1850         M_20# = MClear%     '������
1851         '�G���[�����L�q
1852         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1853         'GOT KEY���͑҂�
1854         MKeyNumber = fnKEY_WAIT()
1855         '
1856         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1857             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1858             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1859             Break
1860         '
1861         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1862             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1863             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1864         '
1865         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1866             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�'MPass%��MNext%�֕ύX(12/7����)
1867             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1868         '
1869         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1870             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1871             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1872             Break
1873         '
1874         EndIf
1875         '
1876         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1877         '
1878     EndIf
1879     '
1880     If M_20# = MContinue% Then *RETRY_PCBCHECK
1881     Exit Function
1882 FEnd
1883 '
1884 '��ScrewTight_S2
1885 ''' <summary>
1886 ''' �˂����߂��s��
1887 ''' </summary>
1888 '''<param name="PScrewPos()">
1889 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1890 '''             PScrewPos(2)    �F�˂����߉��_
1891 '''             PScrewPos(10)   �F�˂����ߏI������
1892 '''</param>
1893 '''<returns>����
1894 '''         0=�ُ�I���A1=����I��
1895 '''</returns>
1896 ''' <remarks>
1897 ''' Date   : 2021/07/07 : M.Hayakawa
1898 ''' </remarks>'
1899 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1900     ScrewTight_S2 = 0
1901     MOKNGFlg = 0
1902     Ovrd 100
1903     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1904     ' �b��
1905     Ovrd 5
1906     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
1907 '    Ovrd MOvrdA
1908     '�b��}�X�N
1909 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1910 '    Dly 0.1
1911 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1912 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
1913 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
1914     ' �b��ړ��̂�
1915     Mvs PScrewPosition(10)
1916 '    '
1917 '    Dly 0.1
1918 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1919 '    Wait M_In(11584)=1          '����/�G���[���o
1920 '    Dly 0.1
1921 '    Spd M_NSpd
1922 '    '
1923 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
1924 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1925 '        Dly 0.1
1926 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1927 '        Dly 0.1
1928 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1929 '        Dly 0.1
1930 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
1931 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1932 '        M_Out(Y68_VV1)=0        '�˂��z���@OFF
1933 '        MOKNGFlg = -1
1934 '        ScrewTight_S2 = 0
1935 '    Else
1936 '        Wait M_In(X29_Driver)=1 ' ���튮����
1937 '        Dly 0.1
1938 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1939 '        Dly 0.1
1940 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
1941 '        Dly 0.1
1942         M_Out(12249)=1 Dly 0.3         '�˂��z���@OFF (�ꎞ�R�����g�A�E�g����,(Y68_VV1)=0��(12249)=1 Dly 0.3�ɕύX(8/5����))
1943         M_Out(12250)=1 Dly 0.1         '�b��^��j��
1944 '        Dly 0.1
1945 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1946 '        ScrewTight_S2 = 1
1947 '    EndIf
1948 ' �b��
1949     Ovrd 10
1950     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1951     Ovrd 100
1952     Exit Function
1953 FEnd
1954 '
1955 '��ScrewGet_S3
1956 ''' <summary>
1957 ''' �˂������@����˂��𓾂�
1958 ''' </summary>
1959 '''<param name="%"></param>
1960 '''         PScrewPos(1)    �F�˂�������̂˂����
1961 '''         PScrewPos(2)    �F�˂���������_
1962 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1963 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1964 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1965 '''<returns>����
1966 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
1967 '''</returns>
1968 ''' <remarks>
1969 ''' Date   : 2021/07/07 : M.Hayakawa
1970 ''' </remarks>'
1971 Function M% ScrewGet_S3(ByVal PScrewPosition())
1972     ScrewGet_S3 = 0
1973     MMScrewJudge% = 0
1974     '�˂������평������G���[�`�F�b�N
1975 ' ���b��폜
1976 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
1977 '    Ovrd 100
1978 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
1979 '        Ovrd 30
1980 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
1981 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
1982 '        M_Out(Y68_VV1)=0    '�˂��z�� Off
1983 '        'NG�Ƃ��Ă����̊֐����甲����
1984 '        ScrewGet_S3 = -1
1985 '        MMScrewJudge% = 1
1986 '        MCommentD1001 = 61
1987 '    EndIf
1988 '    If ScrewGet_S3 = 0 Then
1989 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
1990 '        MMScrewJudge% = 0 'MMScrewJudge������������
1991 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1992 '        If MRtn = 0 Then
1993 '            Ovrd 30
1994 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
1995 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
1996 '            MMScrewJudge% = 2
1997 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
1998 '            MCnt% = 2   '2��ݒ�
1999 '            MCommentD1001 = 62
2000 '        EndIf
2001 '        If MMScrewJudge% = 2 Then
2002 '            ScrewGet_S3 = -2
2003 '        EndIf
2004 '    EndIf
2005 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2006 '    If MMScrewJudge% = 2 Then
2007 '        ScrewGet_S3 = -2
2008 '    EndIf
2009     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2010     Ovrd 100
2011     Spd M_NSpd
2012     If MMScrewJudge% = 0 Then
2013         ScrewGet_S3 = 0
2014         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2015         MScrewCnt% = 0
2016         MFinCnt% = 2
2017 '        For MCnt% = 0 To MFinCnt%
2018             Mov PScrewPosition(2)        ' �˂������@���_
2019             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2020             Ovrd 80
2021             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2022             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2023             Mvs PScrewPosition(10), 1.2
2024             M_Out(Y68_VV1)=1 Dly 0.3        ' �˂��z���@ON
2025             '�r�b�g��]
2026             M_Out(Y60_Driver)=1
2027             Dly 0.2
2028             '
2029             Ovrd 100
2030             JOvrd M_NJovrd
2031             Spd M_NSpd
2032             '�l�W�z���m�F�ʒu�ړ�
2033             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2034             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2035             '�r�b�g��]��~
2036             'M_Out(Y60_Driver)=0
2037             '
2038             '1�b�ԃl�W�z���m�F
2039 ' �ȉ��b��폜
2040 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2041 '            'MRtn = 0'�����G���[
2042 '            '�z���G���[�̏ꍇ
2043 '            '�l�W���˂����Y�ɖ߂�
2044 '            If MRtn = 0 Then
2045 '                Ovrd 30
2046 '                '�r�b�g��]��~
2047 '                M_Out(Y60_Driver)=0
2048 '                '�l�W�����@���
2049 '                Mvs PScrewPos(1)
2050 '                '�X�ɏ��
2051 '                Mov PScrewPos(1), -75
2052 '                '�l�W�̂Ĉʒu
2053 '                Mov PScrewFeedS021
2054 '                '�z��OFF
2055 '                M_Out(Y68_VV1)=0 '�˂��z���@OFF
2056 '                Dly 0.2
2057 '                '�j��ON
2058 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2059 '                '�r�b�g��]
2060 '                M_Out(Y61_Driver)=1
2061 '                Dly 0.5
2062 '                '
2063 '                Ovrd 100
2064 '                JOvrd M_NJovrd
2065 '                Spd M_NSpd
2066 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2067 '                Mov PScrewFeedS021, 10
2068 '                Mov PScrewFeedS021
2069 '                Dly 0.1
2070 '                Mov PScrewFeedS021, 10
2071 '                Mov PScrewFeedS021
2072 '                '
2073 '                '�l�W�����҂�
2074 '                '�r�b�g��]��~
2075 '                M_Out(Y61_Driver)=0
2076 '                Dly 0.1
2077 '                '�j��OFF
2078 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2079 '                '
2080 '                '
2081 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2082 '                Mov PScrewPos(1), -75
2083 '                Ovrd 100
2084 '                Spd M_NSpd
2085 '                '�l�W�����@���
2086 '                Mvs PScrewPos(1)
2087 '                '
2088 '                ScrewGet_S3 = -3
2089 '                Break
2090 '                '
2091 '            Else
2092 '                MCnt% = MFinCnt%
2093 '                ScrewGet_S3 = 0
2094 '            EndIf
2095 '        Next  MCnt%
2096         '
2097         Ovrd 100
2098         Spd M_NSpd
2099         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2100         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2101         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2102         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2103         '������x�z���m�F
2104 ' �ȉ��b��폜
2105 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2106 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2107 '            MCommentD1001 = 94
2108 '            MCommentD1002 = 95
2109 '            ScrewGet_S3 = -3
2110 '        EndIf
2111 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2112 '            ScrewGet_S3 = 1
2113 '        EndIf
2114 '        Break
2115     Else
2116         'M�l�W
2117         If MMScrewJudge% = 2 Then
2118             ScrewGet_S3 = -2
2119         EndIf
2120     EndIf
2121     Exit Function
2122 FEnd
2123 '
2124 '��fnKEY_WAIT()
2125 ''' <summary>
2126 ''' GOT����̃L�[���͑҂�
2127 ''' </summary>
2128 '''<returns>1�F��~    2�F����
2129 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2130 '''         5�FNG
2131 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2132 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2133 '''</returns>
2134 ''' <remarks>
2135 ''' Date   : 2021/07/07 : M.Hayakawa
2136 ''' </remarks>'
2137 Function M% fnKEY_WAIT()
2138     fnKEY_WAIT = 0
2139     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2140     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2141     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2142     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2143     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2144     Dly 0.2
2145     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2146     MLocalLoopFlg=1
2147     While MLocalLoopFlg=1
2148         If M_In(11345) = 1 Then         '��~   M5345
2149             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2150             fnKEY_WAIT = 1
2151             MLocalLoopFlg=-1
2152             Break
2153         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2154             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2155             fnKEY_WAIT = 2
2156             MLocalLoopFlg=-1
2157             Break
2158         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2159             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2160             fnKEY_WAIT = 3
2161             MLocalLoopFlg=-1
2162             Break
2163         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2164             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2165             fnKEY_WAIT = 4
2166             MLocalLoopFlg=-1
2167             Break
2168         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2169             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2170             fnKEY_WAIT = 5
2171             MLocalLoopFlg=-1
2172             Break
2173             '
2174         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2175             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2176             fnKEY_WAIT = MRobotInit1%
2177             MLocalLoopFlg=-1
2178             Break
2179             '
2180         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2181             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2182             fnKEY_WAIT = MRobotInit2%
2183             MLocalLoopFlg=-1
2184             Break
2185             '
2186         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2187             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2188             fnKEY_WAIT = MRobotInit3%
2189             MLocalLoopFlg=-1
2190             Break
2191             '
2192         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2193             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2194             fnKEY_WAIT = MRobotInit4%
2195             MLocalLoopFlg=-1
2196             Break
2197             '
2198         Else
2199         EndIf
2200     WEnd
2201     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2202     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2203     Exit Function
2204 FEnd
2205 '
2206 '�� fnAUTO_CTL
2207 ''' <summary>
2208 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2209 ''' </summary>
2210 ''' <remarks>
2211 ''' Date   : 2021/07/07 : M.Hayakawa
2212 ''' </remarks>
2213 Function M% fnAUTO_CTL
2214     fnAUTO_CTL = 0
2215     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2216     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2217     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2218     '
2219     If M_Svo=0 Then             '�T�[�{ON�m�F
2220         Servo On
2221     EndIf
2222     Wait M_Svo=1
2223     Exit Function
2224 FEnd
2225 '
2226 '�� fnWindScreenOpen
2227 ''' <summary>
2228 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2229 ''' </summary>
2230 '''<param name="%"></param>
2231 '''<param name="%"></param>
2232 '''<param name="%"></param>
2233 '''<param name="%"></param>
2234 ''' <remarks>
2235 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2236 ''' MWindReSet = 0     ��ʔ�\��
2237 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2238 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2239 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2240 ''' Date   : 2021/07/07 : M.Hayakawa
2241 ''' </remarks>
2242 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2243     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2244         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2245     EndIf
2246     '
2247     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2248         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2249     EndIf
2250     '
2251     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2252        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2253     EndIf
2254     '
2255     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2256     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2257     Dly 0.5
2258     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2259     Exit Function
2260 FEnd
2261 '
2262 '��FnCtlValue2
2263 ''' <summary>
2264 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2265 ''' </summary>
2266 ''' <param name="MCtlNo%"></param>
2267 ''' <remarks>
2268 ''' Date : 2022/04/28 �n��
2269 ''' </remarks>
2270 '''
2271 '''  1�F������       �{�P
2272 '''  2�F�g���n�j��   �{�P
2273 '''  3�F�g���m�f��   �{�P (���g�p)
2274 '''  4�F�z���G���[�� �{�P
2275 ''' 99�F�Ǐ��J�n�M�� OFF
2276 '''
2277 Function M% FnCtlValue2(ByVal MCtlNo%)
2278     FnCtlValue2 = 1
2279     Select MCtlNo%
2280         Case 1        '�������{�P
2281             M_Out(12569) = 0             '�����݊J�n�M��OFF
2282             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2283             MInputQty = M_In16(11600)    '��������M
2284             MInputQty = MInputQty + 1    '�������{�P
2285             M_Out16(12592) = MInputQty   '���������M
2286             M_Out(12569) = 1             '�����݊J�n�M��ON
2287             Break
2288             '
2289         Case 2        '�g���n�j���{�P
2290             M_Out(12569) = 0             '�����݊J�n�M��OFF
2291             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2292             MAssyOkQty = M_In16(11616)   '�g��OK����M
2293             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2294             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2295             M_Out(12569) = 1             '�����݊J�n�M��ON
2296             Break
2297             '
2298         Case 4        '�z���G���[���{�P
2299             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2300             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2301             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2302             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2303             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2304             M_Out(12569) = 1                       '�����݊J�n�M��ON
2305             Break
2306             '
2307         Case 99        '�Ǐ��J�n�M��OFF
2308             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2309             M_Out(12569) = 0        '�����݊J�n�M��OFF
2310             Break
2311             '
2312     End Select
2313     Exit Function
2314 FEnd
2315 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2316 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2317 '-------------------------------------------------------------------------------
2318 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2319 '   ����
2320 '       PInspPos()      �F�����ʒu
2321 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2322 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2323 '       MInspCnt%       �F�����ʒu��
2324 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2325 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2326 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2327 '   �߂�l�F����
2328 '       0=�ُ�I���A1=����I��
2329 '
2330 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2331 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2332 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2333 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2334 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2335 '-------------------------------------------------------------------------------
2336     '----- �����ݒ� -----
2337     Cnt 0                                                           '�ړ�����������(�����l=0)
2338     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2339 '    Cnt 1,0.1,0.1
2340     '�ϐ��錾�E������
2341     Def Inte MNum                                                   '�����ԍ�(������1�`)
2342     MNum% = 1                                                       '�����ԍ������l�ݒ�
2343     Def Inte MEndFlg                                                '�����I���t���O
2344     MEndFlg% = 0
2345     '
2346     '����G�ԍ��ݒ�v���E�������s�v��off
2347     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2348     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2349     '�G���[�ԍ��N���A
2350     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2351     M_Out16(MOUT_InspErrNum) = MInspErrNum
2352     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2353     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2354     '
2355     'Insight Ready check?
2356     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2357         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2358         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2359         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2360         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2361         Exit Function
2362     EndIf
2363     '
2364     '�����ʒu���m�F
2365     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2366         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2367         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2368         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2369         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2370         Exit Function
2371     EndIf
2372     '
2373     '
2374     '
2375     '----- ���C������ -----
2376     '�ݒ肳�ꂽ�����ʒu�����̌������s
2377     While( MEndFlg% = 0 )
2378         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2379         MSetGrNumRetryExitFlg = 0
2380         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2381         While( MSetGrNumRetryExitFlg = 0 )
2382         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2383             '
2384             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2385             '
2386             '----- �����O���[�v�ԍ��ݒ� -----
2387             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2388             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2389             '
2390             '�����ʒu�ֈړ��E�ړ������҂�
2391             Mvs PInspPos( MNum% )                                       '�ړ�
2392             Dly 0.05                                                    '�ړ�������Delay
2393             '
2394             '�����O���[�v�ԍ��ݒ�I���m�F
2395             M_Timer(1) = 0
2396             MExitFlg = 0
2397             While( MExitFlg = 0 )
2398                 '����G�ݒ萳��I��?
2399                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2400                     MExitFlg = 1
2401                 '
2402                 '����G�ݒ�ُ�I��?
2403                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2404                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2405                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2406                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2407                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2408                     EndIf
2409                     MExitFlg = 1
2410                 '
2411                 'timeout�`�F�b�N
2412                 ElseIf 1000 < M_Timer(1) Then
2413                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2414                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2415                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2416                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2417                     EndIf
2418                     MExitFlg = 1
2419                 EndIf
2420             WEnd
2421             '
2422             '����G�ԍ��ݒ�v��off
2423             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2424             '
2425             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2426             'NG�Ȃ���Δ�����
2427             If MCurrentStepErr = 0 Then
2428                 MSetGrNumRetryExitFlg = 1
2429             Else
2430                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2431                 If MSetGrNumRetryCnt = 0 Then
2432                     MSetGrNumRetryExitFlg = 1
2433                 Else
2434                     'Retry�ց@���̑O��Delay
2435                     Dly 0.5
2436                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2437                 EndIf
2438             EndIf
2439             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2440             '
2441         WEnd
2442         '
2443         '
2444         '
2445         '----- �������s -----
2446         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2447             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2448                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2449                 MInspRetryExitFlg = 0
2450                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2451                 While( MInspRetryExitFlg = 0 )
2452                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2453                     '
2454                     '���������m�F
2455                     MRetryCnt = MRetryCnt - 1
2456                     M_Timer(1) = 0
2457                     MExitFlg = 0
2458                     While( MExitFlg = 0 )
2459                     '���������҂�
2460                         '����OK�I��?
2461                         If M_In( MIN_IS_InspOK% ) = 1  Then
2462                             MJudgeOKFlg = 1                         '����OK�t���OON
2463                             MExitFlg = 1
2464                         '
2465                         '����NG�I��?
2466                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2467                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2468                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2469                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2470                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2471                                 EndIf
2472                             EndIf
2473                             MExitFlg = 1
2474                         '
2475                         '�����ُ�I��(IS timeout)?
2476                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2477                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2478                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2479                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2480                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2481                                 EndIf
2482                             EndIf
2483                             MExitFlg = 1
2484                         '
2485                         'timeout�`�F�b�N
2486                         ElseIf 3000 < M_Timer(1) Then
2487                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2488                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2489                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2490                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2491                                 EndIf
2492                             EndIf
2493                             MExitFlg = 1
2494                         EndIf
2495                     WEnd
2496                     '
2497                     '�����J�n�v��off
2498                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2499                     '
2500                     'OK�Ȃ甲����
2501                     If MJudgeOKFlg = 1 Then
2502                         MInspRetryExitFlg = 1
2503                     Else
2504                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2505                         If MRetryCnt = 0 Then
2506                             MInspRetryExitFlg = 1
2507                         Else
2508                             'Retry�ց@���̑O��Delay
2509                             Dly 0.3
2510                         EndIf
2511                     EndIf
2512                     '
2513                 WEnd
2514             EndIf
2515         EndIf
2516         '
2517         '
2518         '
2519         MNum% = MNum% + 1                                           '����Step+1
2520         '�����I���m�F�@�����I���t���O�Z�b�g
2521         If (MInspCnt% < MNum% ) Then
2522             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2523         EndIf
2524         'NG���������s������
2525         If MInspErrNum <> 0 Then                                    'NG����?
2526             If MNgContinue% <> 1 Then                               'NG���s?
2527                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2528             EndIf
2529         EndIf
2530     WEnd
2531     '
2532     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2533     If 0 < MZAxis% Then
2534         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2535         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2536         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2537     EndIf
2538     '
2539     '�߂�l�ݒ�
2540     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2541         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2542     Else
2543         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2544         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2545         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2546     EndIf
2547     '
2548     Fine 0 , P
2549     Exit Function
2550 FEnd
2551 '
2552 '��fnAutoScreenComment
2553 ''' <summary>
2554 ''' ���C����ʂ̓���󋵕\��
2555 ''' �R�����gD1005�̐ݒ�
2556 ''' </summary>
2557 '''<param name="McommentD1005%">�R�����gID</param>
2558 ''' <remarks>
2559 ''' Date   : 2021/07/07 : M.Hayakawa
2560 ''' </remarks>
2561 Function fnAutoScreenComment(ByVal McommentD1005%)
2562     M_Out16(12576) = McommentD1005%
2563     Exit Function
2564 FEnd
2565 '
2566 '��InitialZoneB
2567 ''' <summary>
2568 ''' ����~��̕��A����
2569 ''' 1)���ޔ��@Z������Ɉړ�
2570 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2571 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2572 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2573 ''' </summary>
2574 ''' <remarks>
2575 ''' Date : 2022/04/08 : N.Watanabe
2576 ''' </remarks>
2577 Function V fnInitialZoneB()
2578     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/27 �n��
2579 '
2580 '�p�����[�^
2581     Ovrd 5
2582 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2583 '    Cmp Pos, &B100011
2584 '
2585 '���A����J�n
2586 '
2587 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2588 *RecoveryChuckOpen
2589     PActive = P_Curr          '���݈ʒu���擾
2590     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2591 'PProductOnRoboSet(�˂����{1�{�̒u���ʒu)�́A�`���b�N���
2592     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2593         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2594             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2595                 MRecoveryChuckOpen = 1
2596             EndIf
2597         EndIf
2598     EndIf
2599 'PProductOnRoboGet(�˂����{1�{�̎��ʒu)�́A�`���b�N���
2600     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2601         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2602             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2603                 MRecoveryChuckOpen = 1
2604             EndIf
2605         EndIf
2606     EndIf
2607 '
2608     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2609     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2610     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2611 '
2612     M_20# = 0                                  'KEY���͏�����
2613     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2614     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2615     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2616 '
2617     fErrorProcess(11,244,284,0)
2618     If M_20# = MNext% Then M_20# = MClear%
2619     If M_20# = MAbout% Then GoTo *RecoveryEnd
2620     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2621     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2622 '
2623     *RecoveryChuckOpenEnd
2624 '
2625 '���ޔ�
2626     PActive = P_Curr
2627     Pmove = PActive
2628     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
2629     If PActive.X > 550 Then
2630         Pmove.Z =480        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
2631     EndIf
2632     If PActive.Z < Pmove.Z Then   '���݂̍�����Pmove���Ⴂ�Ƃ̂ݎ��s
2633         Mvs Pmove
2634     EndIf
2635 '
2636     Dly 1.0
2637 'J1���ȊO��ޔ��|�W�V�����ֈړ�
2638     JActive = J_Curr
2639     Jmove = JTaihi
2640     Jmove.J1 = JActive.J1        'J1���̂݌��ݒl���g�p���A���̎���JTaihi�̃|�[�Y�����
2641     Mov Jmove
2642     Dly 1.0
2643 'J1���݂̂�ޔ��|�W�V�����ֈړ�
2644     Mov JTaihi
2645     Dly 1.0
2646 '�C�j�V�����|�W�V�����ֈړ�
2647     Mov PInitialPosition
2648     Cmp Off
2649 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
2650     If M_In(11856) = 0 Then                 ' ��~���̂�
2651         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/27 �n��
2652         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
2653         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
2654         If MRet = 0 Then
2655         Else
2656             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
2657         EndIf
2658     EndIf
2659     M_Out(12264) = 0            '�ʒu���ߏoOFF
2660     M_Out(12265) = 1            '�ʒu���ߖ�ON
2661    fErrorProcess(11,253,281,0)
2662 *RecoveryEnd
2663     Exit Function
2664 FEnd
2665 '
2666 '
2667 '��fnRoboPosChk
2668 ''' <summary>
2669 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2670 ''' </summary>
2671 '''<param name="MINNumber%">���͔ԍ�</param>
2672 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2673 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2674 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2675 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2676 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2677 ''' <remarks>
2678 ''' Date   : 2021/07/07 : M.Hayakawa
2679 ''' </remarks>
2680 Function M% fnRoboPosChk
2681     fnRoboPosChk = 0
2682     MRet = fnStepRead()
2683     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2684     '�E�B���h��ʐ؊���
2685     If MRBTOpeGroupNo > 5 Then
2686         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2687         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2688         Dly 0.2
2689         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2690         Dly 1.5
2691         '
2692         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2693         '
2694         MLoopFlg% = 1
2695         While MLoopFlg% = 1
2696             '
2697             '
2698             MKeyNumber% = fnKEY_WAIT()
2699             Select MKeyNumber%
2700                 Case Is = MAbout%       '��~
2701                     M_20# = MAbout%
2702                     MLoopFlg% = -1
2703                     Break
2704                 Case Is = MNext%        '����
2705                     'MLoopFlg% = -1
2706                     Break
2707                 Case Is = MContinue%    '�p��
2708                     M_20# = MContinue%
2709                     MLoopFlg% = -1
2710                     Break
2711                 Default
2712                     Break
2713             End Select
2714         WEnd
2715     EndIf
2716     '
2717     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2718         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2719         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2720         Select MRBTOpeGroupNo
2721             Case Is = 5                          '�������Ȃ�
2722                 Break
2723             Case Is = 10                         '�����ʒu�֖߂�
2724                 'Mov PTEST001
2725                 Break
2726             Case Is = 15                         '�����ʒu�֖߂�
2727                 'Mov PTEST002
2728                 Dly 0.5
2729                 'Mov PTEST001
2730                 Dly 0.5
2731                 Break
2732             Default
2733                 Break
2734         End Select
2735         '
2736         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2737         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2738         MRBTOpeGroupNo = 5
2739         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2740         Dly 1.0
2741         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2742         fnRoboPosChk = 1                        '�����ʒu������s
2743         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2744     EndIf
2745     Exit Function
2746 FEnd
2747 '
2748 '��frInCheck
2749 ''' <summary>
2750 ''' �Z���T�[IN�`�F�b�N
2751 ''' </summary>
2752 '''<param name="MINNumber%">���͔ԍ�</param>
2753 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2754 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2755 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2756 ''' <remarks>
2757 ''' Date   : 2021/07/07 : M.Hayakawa
2758 ''' </remarks>
2759 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2760     M_Timer(4) = 0
2761     MloopFlg = 0
2762     While MloopFlg = 0
2763         MCrtTime& = M_Timer(4)
2764         If M_In(MINNumber%) = MCMPFLG% Then
2765             MloopFlg = 1
2766             frInCheck = 1
2767         ElseIf MCrtTime& > MTimeCnt& Then
2768             MloopFlg = 1
2769             frInCheck = 0
2770         EndIf
2771     WEnd
2772     Exit Function
2773 FEnd
2774 '-----------------------------------------------
2775 '
2776 '�˂����ߋ@�ʐM�m�F
2777 '
2778 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2779 'fScewTcomChk = 0�@�F����I��
2780 '          �@�@ -1 �F�ُ�I��
2781 '-----------------------------------------------
2782 Function M% fScewTcomChk
2783 *ReCheckScewTcomChk
2784     fScewTcomChk = 0
2785     '�ʐM�m�F���M
2786     M_Out(MOUT_ScwT_ComChk%) = MOn%
2787     '�ʐM�m�F��M�ҋ@
2788 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2789     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2790     '�ʐM�m�F���M�I��
2791     M_Out(MOUT_ScwT_ComChk%) = MOff%
2792     If MRtn = 0 Then
2793         fScewTcomChk = -1
2794     EndIf
2795     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2796  '
2797 FEnd
2798 '
2799 '
2800 '-----------------------------------------------
2801 '
2802 '�˂����ߊJ�n���M
2803 '
2804 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2805 'fScewTStart = 0�@�F����I��
2806 '          �@�@-1 �F�ُ�I��
2807 '-----------------------------------------------
2808 Function M% fScewTStart
2809     fScewTStart = 0
2810     nRet% = 0
2811     '�˂����ߊJ�n�ҋ@����M
2812 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2813     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2814     If MRtn = 0 Then nRet% = -1
2815     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
2816     Dly 0.1
2817     '�˂����ߊJ�n��M�𑗐M
2818     M_Out(MOUT_ScwT_ST%) = MOn%
2819     Dly 0.5
2820     'Wait M_In(MTEST_KEY%) = MOn%
2821     '�˂����ߊJ�n���M�I��
2822     M_Out(MOUT_ScwT_ST%) = MOff%
2823     '
2824 *ScrewStartERROR
2825     fScewTStart = nRet%
2826 FEnd
2827 '
2828 '
2829 '
2830 '-----------------------------------------------
2831 '
2832 '�˂����ߊ�����M
2833 '
2834 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2835 'fScewTcomChk = 0�@�F����I��
2836 '          �@ �@-1 �F�ُ�I��
2837 '-----------------------------------------------
2838 Function M% fScewTFinish
2839 *ReCheckScewTFinish
2840     fScewTFinish = 0
2841     '�˂����ߊ����ҋ@����M
2842 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
2843     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
2844     If MRtn = 0 Then
2845         fScewTFinish = -1
2846     EndIf
2847     If MRtn = 2 Then GoTo *ReCheckScewTFinish
2848     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
2849     Dly 0.1
2850     '�˂����ߊ�����M�𑗐M
2851     M_Out(MOUT_ScwT_FinOK%) = MOn%
2852     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
2853     '�˂����ߊJ�n���M�I��
2854     M_Out(MOUT_ScwT_FinOK%) = MOff%
2855     'Wait M_In(MTEST_KEY%) = MOn%
2856     '
2857 *ScewTFinish_ErrEnd
2858 FEnd
2859 '
2860 '
2861 '-----------------------------------------------
2862 '
2863 '����xx��~��M
2864 '
2865 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2866 'fScewTCaseStop = 0�@�F����I��
2867 '          �@   �@-1 �F�ُ�I��
2868 '-----------------------------------------------
2869 Function M% fScewTCaseStop(ByVal MCase%())
2870 *ReCheckScewTCaseStop
2871     fScewTCaseStop = 0
2872     '����xx��~����M
2873     Wait M_In(MCase%(1)) = MOn%
2874     MRtn = fTimeOutJudge(MCase%(1),MOn%)
2875     If MRtn = 0 Then
2876         fScewTCaseStop = -1
2877     EndIf
2878     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
2879     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
2880     Dly 0.1
2881     '����xx��~��M�𑗐M
2882     M_Out(MCase%(2)) = MOn%
2883     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
2884     '�˂����ߊJ�n���M�I��
2885     M_Out(MCase%(2)) = MOff%
2886 *ScewTCaseStop_ErrEnd
2887     '
2888 FEnd
2889 '
2890 '-----------------------------------------------
2891 '
2892 '�ĊJ�n��M
2893 '
2894 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2895 'fScewTReStart = 0�@�F����I��
2896 '              �@-1 �F�ُ�I��
2897 '-----------------------------------------------
2898 Function M% fScewTReStart()
2899 *ReCheckScewTReStart
2900     fScewTReStart = 0
2901     '�ĊJ�n����M
2902     Wait M_In(MIN_ScwT_ReST%) = MOn%
2903     MRtn = fTimeOutJudge(MIN_ScwT_ReST%,MOn%)
2904     If MRtn = 2 Then GoTo *ReCheckScewTReStart
2905     If MRtn = 0 Then GoTo *ScewTReStart_ErrEnd
2906     Dly 0.1
2907     '�ĊJ�n��M�𑗐M
2908     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
2909 *ScewTReStart_ErrEnd
2910     Exit Function
2911 FEnd
2912 '
2913 '��fScrewTighenRoboCheck
2914 '<summary>
2915 '�˂����{�Ď�
2916 '</summary>
2917 '<param name = "MStopNum%"> ��~�ԍ�</param>
2918 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
2919 '<make>
2920 '2021/12/2 �����V��
2921 '</make>
2922 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
2923     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/27 �n��
2924     fScrewTighenRoboCheck = 1
2925     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
2926     MCheck% = 0
2927     While MScrewTighenRoboFlg% = 1
2928         MCheck% = M_In16(11904)
2929         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
2930             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
2931         EndIf
2932         If MCheck% <> 0 Then
2933             fScrewTighenRoboError(MCheck%)
2934             Select M_20#
2935                 Case MAbout%            '��~�������ꂽ�ꍇ
2936                     M_Out(12869) = 1 Dly 1.0
2937                     MScrewTighenRoboFlg% = 0
2938                     fScrewTighenRoboCheck = 0   '�ُ�I��
2939                     Break
2940                 Case MNgProcess%        'NG�������ꂽ�ꍇ
2941                     M_Out(12873) = 1 Dly 1.0
2942                     MScrewTighenRoboFlg% = 0
2943                     fScrewTighenRoboCheck = 0   '�ُ�I��
2944                     Break
2945                 Case MContinue%             '���g���C�������ꂽ�ꍇ
2946                     M_20# = MClear%         'M_20#������
2947                     M_Out(12871) = 1 Dly 1.0
2948                     Break
2949                 Case MNext%                 '���ւ������ꂽ�ꍇ
2950                     M_20# = MClear%         'M_20#������
2951                     M_Out(12874) = 1 Dly 1.0
2952                     Break
2953             End Select
2954             Dly 0.5
2955         EndIf
2956     WEnd
2957     Exit Function
2958 FEnd
2959 '��fScrewTighenRoboError
2960 '<summary>
2961 '�˂����{�G���[����
2962 '</summary>
2963 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
2964 '<make>
2965 '2021/12/2 �����V��
2966 '</make>
2967 Function fScrewTighenRoboError(ErrorCode%)
2968     MCommentD1001 = ErrorCode% + 300
2969     fErrorProcess(11,MCommentD1001,0,0)
2970     Exit Function
2971 FEnd
2972 '��fErrorProcess
2973 '<summary>
2974 '�G���[����
2975 '</summary>
2976 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2977 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2978 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2979 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2980 '<make>
2981 '2021/11/5 �����V��
2982 '</make>
2983 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2984     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2985     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2986     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2987     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2988 *RETRY_ERR_PROCESS
2989      M_20# = MClear%     '������
2990 '        '�G���[�����L�q
2991         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2992 '        'GOT KEY���͑҂�
2993         MKeyNumber = fnKEY_WAIT()
2994 '        '
2995         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2996             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2997  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2998             Break
2999          '
3000         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3001             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3002  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3003             Break
3004         '
3005         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3006             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3007  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3008             Break
3009          '
3010         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3011             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3012  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3013             Break
3014         '
3015         EndIf
3016         '
3017         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3018         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3019     Exit Function
3020 FEnd
3021 '
3022 '��fnTorqueCheck
3023 ''' <summary>
3024 ''' �g���N�`�F�b�N����p�̃��C��
3025 ''' </summary>
3026 ''' <remarks>
3027 ''' Date   : 2021/12/21 : H.AJI
3028 ''' </remarks>'
3029 Function M% fnTorqueCheck
3030     '�g���N�`�F�b�N�����M  �����n��~
3031     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3032     '
3033     fnTorqueCheck = 0
3034     Ovrd 20
3035     'Mov PInitialPosition              '�����ʒu�ړ�
3036     Ovrd 100
3037     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3038     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3039     Dly 0.2
3040     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3041     '
3042     'M6340  �g���N�`�F�b�N��M
3043     'Dly 5.0
3044     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3045     Dly 1.0
3046     M_Out(12340) = 0
3047     '
3048     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3049     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3050    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3051     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3052     '
3053     '
3054     MLoopFlg = 1
3055     While MLoopFlg = 1
3056         '
3057         'Mov PInitialPosition              '�����ʒu�ړ�
3058         '
3059         MKeyNumber = fnKEY_WAIT()
3060         Select MKeyNumber
3061             Case Is = 1           '��~
3062                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3063                 Dly 1.0
3064                 M_Out(12343) = 0
3065                 Ovrd 20
3066                 'Mov PTicketRead_1
3067                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3068                 Wait M_In(11859) = 1      '�˂����{����̏I��
3069                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3070                 Ovrd 100
3071                 M_20# = 1
3072                 MLoopFlg = -1
3073                 Break
3074             Case Is = 2           '����
3075                 Break
3076             Case Is = 3           '�p��
3077                 Break
3078             Case Is = 4           '�g���N�`�F�b�N�J�n
3079                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3080                 Dly 1.0
3081                 M_Out(12342) = 0
3082                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3083                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3084                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3085                 EndIf
3086                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3087                 'MRet = fnMoveTorquePosi()
3088                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3089                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3090                 Break
3091             Default
3092                 Break
3093         End Select
3094     WEnd
3095     '
3096     '�g���N�`�F�b�N����~���M
3097     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3098     '
3099     '���{�b�g�̈ʒu�����ɖ߂�
3100     '
3101     Exit Function
3102  FEnd
3103  '
3104 '
3105 '
3106 '---------------------------
3107 '
3108 '    ���C����ʂ̕\���A��\���ݒ�
3109 '         �R�����gD1001, D1002, D1003�̐ݒ�
3110 '           MWindReSet = 0     ��ʔ�\��
3111 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3112 '           MWindErrScr = 10    �G���[��� D1001, D1002
3113 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3114 '
3115 '---------------------------
3116 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3117     fnMainScreenOpen = 0
3118     '
3119    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3120         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3121     EndIf
3122     '
3123     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3124         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3125     EndIf
3126     '
3127     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3128         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3129     EndIf
3130     '
3131     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3132     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3133     Dly 0.5
3134     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3135     Exit Function
3136 FEnd
3137 '
3138 '��Main
3139 ''' <summary>
3140 ''' �g���N�`�F�b�N������
3141 ''' </summary>
3142 ''' <remarks>
3143 ''' Date   : 2021/12/21 : H.AJI
3144 ''' </remarks>'
3145 Function M% fnScrewMTorque
3146     fnScrewMTorque = 0
3147     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3148     Wait M_In(11857) = 1                     '��M����
3149     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3150     Dly 2.0
3151     Exit Function
3152 FEnd
3153 '
3154 '
3155 '----------------------------------------------------------------
3156 'fTimeOutJudge
3157 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3158 '����
3159 'Address% = �Ď��A�h���X�ԍ�
3160 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3161 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3162 '�߂�l = 0 �G���[
3163 '         1 ����I��
3164 '         2 ���g���C
3165 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3166 '�쐬��
3167 '2022/9/20 ����
3168 '----------------------------------------------------------------
3169 '
3170 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3171     fTimeOutJudge = 0
3172     MJudge% = 1
3173     MRtn = 0
3174     M_20# = MClear%
3175     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3176 *TimeOutLoop
3177     If MRtn = 1 Then GoTo *TimeOut
3178         fErrorProcess(11,202,203,0)
3179         If M_20# = MNext% Then GoTo *TimeOutLoop
3180         If M_20# = MContinue% Then MJudge% = 2
3181         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3182 *TimeOut
3183     fTimeOutJudge = MJudge%
3184 '
3185 *JUDGE_ERROR_END
3186 FEnd
3187 '
3188 '��Main
3189 ''' <summary>
3190 ''' �g������p�̃��C��
3191 ''' </summary>
3192 ''' <remarks>
3193 ''' Date   : 2021/07/07 : M.Hayakawa
3194 ''' </remarks>'
3195 Function Main
3196     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3197     '
3198     If M_Svo=0 Then
3199         Servo On
3200     EndIf
3201     Wait M_Svo=1
3202 '�g���X�^�[�g���t�����v���p���XON
3203     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3204 '�p�g���C�g����
3205     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3206     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3207     '
3208     M_20# = 0                                   'KEY���͏�����
3209     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3210     MRet% = 0
3211 '�����ʒu�̊m�F�ƈړ�
3212 '
3213 '���A����@���s�E�����s����      2022/04/08 �n�� �쐬
3214     PActive = P_Curr                    '���݈ʒu���擾
3215     MRecoveryPass% = 0
3216     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3217         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3218             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3219                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3220             EndIf
3221         EndIf
3222     EndIf
3223     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3224         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3225             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3226                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3227             EndIf
3228         EndIf
3229     EndIf
3230     If MRecoveryPass% = 0 Then
3231        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3232     EndIf
3233 '
3234 '
3235 '    MRet% = fnRoboPosChk()
3236     If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ
3237         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3238         MKeyNumber% = fnKEY_WAIT()
3239         Select MKeyNumber%
3240             Case Is = MAbout%       '��~
3241                 M_20# = MAbout%
3242                 MLoopFlg% = -1
3243                 Break
3244             Case Is = MNext%        '����
3245                 'MLoopFlg = -1
3246                 Break
3247             Case Is = MContinue%    '�p��
3248                 M_20# = MContinue%
3249                 MLoopFlg% = -1
3250                 Break
3251             Default
3252                 Break
3253         End Select
3254     EndIf
3255     '
3256     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3257         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3258 '�g���N�`�F�b�N
3259         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3260             MRet% = fnTorqueCheck()
3261             Break
3262         Else
3263 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3264 '                MRtn = InspInit()               '�摜��������������
3265 '            EndIf
3266             '
3267            M_20# = MClear%                    '������
3268 '�g���J�n
3269             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3270                 MRet% = fnAssyStart()
3271             Else
3272                 M_20# = MPass%
3273             EndIf
3274 '�g���I�����t����
3275             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3276             Wait M_In(11572) = 1            '���t�擾����
3277             Dly 0.1
3278             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3279 '���t�^�[���j�b�g�ւ�OUT
3280             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3281             fnAutoScreenComment(89)         'AUTO��� �g����������
3282             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3283 'OK/NG�t���O�o��
3284             If M_20# <= 0 Then
3285                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3286             ElseIf M_20# = MPass% Then
3287                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3288             EndIf
3289 'PIAS�ɑg������������
3290             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3291                 If M_20# = MPass% Then
3292                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3293                 Else
3294                     'KEY���͂�NG�̏ꍇ
3295                     If M_20# = MNgProcess% Then
3296                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3297                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3298                         MRet% = fnPiasWrite(MNG%)
3299                        nAssyNgQty = nAssyNgQty + 1
3300                     EndIf
3301                     '
3302                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3303                     If M_20# = MAssyOK% Then
3304                             '-----------------------
3305                             'D732 -> D2600 �R�s�[�v��
3306                             M_Out(12566) = 1
3307 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3308                             M_Out(12566) = 0
3309                             '
3310                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3311                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3312                             '��ԍ��ƍ�(PP�͖��g�p�j
3313 '                            MRet% = fnPCBNumberCheck()
3314                         Else
3315                             MRet% = 1
3316                         EndIf
3317                         '
3318                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3319                             If M_20# <> MAbout% Then
3320                                 '�H������OK��������
3321                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3322                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3323                                 MRet% = fnPiasWrite(MOK%)
3324                                 nAssyOkQty = 0
3325                                 nAssyOkQty = nAssyOkQty + 1
3326                             Else
3327                                 nAssyOkQty = nAssyOkQty + 1
3328                             EndIf
3329                         EndIf
3330                     EndIf
3331 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3332 '                    MRet% = fnPiasWrite(MOK%)
3333                 EndIf
3334             Else
3335                 nAssyOkQty = nAssyOkQty + 1
3336             EndIf
3337             '
3338             '�g���I�����t��������
3339             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3340             '�������A�g��OK���A�g��NG��������
3341 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3342             '
3343 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3344 '                '�摜�����I������
3345 '                MRtn = InspQuit()
3346 '            EndIf
3347         EndIf
3348         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3349     EndIf
3350 '�p�g���C�g����
3351     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3352     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3353 'GOT�\��
3354     fnAutoScreenComment(93)  'AUTO��� �H������
3355 FEnd
3356 End
3357 '
3358 '���܂��Ȃ��R�����g
3359 '��΍폜�����
3360 '
3361 ''
3362 '
3363 '
3364 '
PInspPosition(1)=(-53.94,-368.56,+601.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(-53.94,-598.55,+601.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PTemp=(+599.21,-290.97,+540.00,+180.00,+0.00,-90.00,+0.00,+0.00)(7,0)
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
PActive=(+599.21,-290.97,+540.00,+180.00,+0.00,-90.00,+0.00,+0.00)(7,0)
Pmove=(+313.97,-375.28,+640.00,+179.97,-0.46,-178.25,+0.00,+0.00)(7,0)
PInitialPosition=(+300.00,+0.00,+540.00,+180.00,+0.00,+180.00)(7,0)
PPlateLCheck=(-53.94,-598.55,+601.00,-180.00,+0.00,+90.00)(7,0)
PPlateLCheck_2=(-53.94,-598.55,+631.00,-180.00,+0.00,+90.00)(7,0)
PPlateLGet=(+639.88,-162.76,+441.80,+179.52,-0.10,+91.51)(7,0)
PPlateLGet_1=(+639.88,-162.76,+480.00,+179.52,-0.10,+91.51)(7,0)
PPlateLGet_2=(+396.00,-160.55,+570.00,+179.49,+0.10,+91.01)(7,0)
PPlateLSet=(+49.07,-583.23,+542.63,+179.97,-0.46,-178.26)(7,0)
PPlateLSet_1=(+49.07,-583.23,+600.00,+179.97,-0.46,-178.26)(7,0)
PPlateLSet_2=(+44.56,-280.00,+640.60,+179.98,-0.12,-179.36)(7,0)
PPlateRCheck=(-53.94,-368.56,+601.00,-180.00,+0.00,+90.00)(7,0)
PPlateRCheck_2=(-53.94,-368.56,+631.00,-180.00,+0.00,+90.00)(7,0)
PPlateRGet=(-510.99,+86.93,+362.39,-179.41,+0.80,+2.22)(7,15)
PPlateRGet_1=(-510.99,+86.93,+400.00,-179.41,+0.80,+2.22)(7,15)
PPlateRGet_2=(-323.09,-0.06,+640.58,+179.98,-0.13,+1.25)(7,0)
PPlateRGet_3=(+44.59,-320.00,+640.61,+179.98,-0.13,+99.17)(7,0)
PPlateRSet=(+44.84,-526.60,+546.32,+179.99,+0.61,-179.14)(7,0)
PPlateRSet_1=(+44.84,-526.60,+570.00,+179.99,+0.61,-179.14)(7,0)
PPlateRSet_2=(+45.33,-320.00,+640.00,+180.00,+0.00,-178.68)(7,0)
PPlateRSet_3=(+0.01,-336.64,+640.48,+180.00,-0.01,+90.00)(7,0)
PProductOnPltGet=(+547.57,-99.63,+413.85,-180.00,+0.00,-179.49)(7,0)
PProductOnPltGet_1=(+547.57,-99.63,+460.00,-180.00,+0.00,-179.49)(7,0)
PProductOnPltGet_2=(+547.57,-99.63,+530.00,-180.00,+0.00,-179.49)(7,0)
PProductOnPltSet=(+547.59,-97.90,+414.00,-180.00,+0.00,-179.52)(7,0)
PProductOnPltSet_1=(+547.59,-97.90,+460.00,-180.00,+0.00,-179.52)(7,0)
PProductOnPltSet_2=(+547.59,-97.90,+530.00,-180.00,+0.00,-179.52)(7,0)
PProductOnRoboGet=(+102.69,-556.31,+467.49,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboGet_1=(+102.69,-556.31,+500.00,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboGet_2=(+102.69,-556.31,+640.00,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboSet=(+102.66,-556.32,+468.69,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboSet_1=(+102.66,-556.32,+510.00,+179.84,+0.03,-179.68)(7,0)
PProductOnRoboSet_2=(+102.66,-556.32,+570.00,+179.84,+0.03,-179.68)(7,0)
PTicketRead=(+599.21,-290.97,+473.00,-180.00,+0.00,-90.00)(7,0)
PTicketRead_1=(+599.21,-290.97,+540.00,-180.00,+0.00,-90.00)(7,0)
JActive=(-50.02,+20.03,+76.82,-0.35,+83.46,-51.73,+0.00,+0.00)
Jmove=(-50.02,-9.85,+108.99,+1.35,+80.50,+0.00,+0.00,+0.00)
JTaihi=(+0.00,-9.85,+108.99,+1.35,+80.50,+0.00,+0.00,+0.00)
