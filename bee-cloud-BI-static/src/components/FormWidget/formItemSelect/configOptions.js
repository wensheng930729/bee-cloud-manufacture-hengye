export const configOptions = {
  state: [{ value: 'pause', label: '运行' }],
  sex: [{ value: '0', label: '未知' }, { value: '1', label: '男' }, { value: '2', label: '女' }],
};

const noRequired = { required: false, message: '必填' };

export const configRule = {
  required: [{ required: true, message: '必填' }],
  normal: [noRequired],
  Integer: [
    noRequired,
    // {type:'integer',message:'请输入整数'},
  ],
  Number: [
    noRequired,
    // {type:'number',message:'请输入数字'},
    // {max:'100',min:'1',message:'最长100,最短1'},
  ],
  BigNumber: [
    noRequired,
    // {max:'300',min:'1',message:'最长100,最短1'},
  ],
  String: [
    noRequired,
    // {type:'string',message:'请输入字符串'},
    // {max:'100',min:'1',message:'最长100字符长度,最短1字符长度'},
  ],
  Binary: [
    noRequired,
    // {max:'100',min:'1',message:'最长100字节长度,最短1字节长度'},
  ],
};
