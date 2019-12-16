import { Component } from 'react';
import { Button, Card, Form, Input, Select, DatePicker, Table, InputNumber, message } from 'antd';
import styles from './index.less';
import router from 'umi/router';
import NewUpload from './components/NewUpload';
import {
  getSupplierListByCategory,
  getProducts,
  getLocationList,
  addContractBuy,
} from './services/services';
import moment from 'moment';

const FormItem = Form.Item;
const Option = Select.Option;
const formatter = 'YYYY-MM-DD';

@Form.create()
export default class Add extends Component {
  state = {
    suppliers: [],
    products: [],
    locations: [],
    proInfo: [
      {
        productId: undefined,
        productName: undefined,
        qualityRequirement: undefined,
        quantity: undefined,
        unitPrice: undefined,
      },
    ],
  };

  componentDidMount() {
    getSupplierListByCategory().then(res => {
      if (res.code === 1) {
        this.setState({ suppliers: res.object });
      }
    });
    getProducts().then(res => {
      if (res.code === 1) {
        this.setState({ products: res.object });
      }
    });
    getLocationList().then(res => {
      if (res.code === 1) {
        this.setState({ locations: res.object });
      }
    });
  }

  saveProInfo = values => {
    let { proInfo } = this.state;
    proInfo[0] = {
      ...proInfo[0],
      ...values,
    };
    this.setState({
      proInfo,
    });
  };

  handleOnChange = ({ file, fileList }) => {
    if (file.status === 'error') {
      message.error('合同文件上传异常，请重试');
      return [];
    }
    return fileList.map(item => ({
      uid: item.uid,
      name: item.name,
      status: 'done',
      url: item.response ? item.response.object.access_url : item.url,
    }));
  };

  handleAdd = () => {
    try {
      _czc1.push(['_trackEvent', '采购订单', '新增', '', '', '']);
    } catch (error) {}

    const { validateFields } = this.props.form;
    const { proInfo } = this.state;

    validateFields((err, values) => {
      if (!err) {
        let newValues = [];
        let files = [];

        if (values.files && values.files.length) {
          values.files.forEach(item => {
            files.push({
              fileName: item.name,
              fileUrl: item.url,
            });
          });
        }

        if (proInfo[0].productId === undefined || proInfo[0].productName === undefined) {
          return message.warning('请选择产品');
        }

        newValues = {
          ...values,
          files,
          signDate: moment(values.signDate).format(formatter),
          ...proInfo[0],
        };

        addContractBuy(newValues).then(res => {
          if (res.code === 1) {
            message.success('新增成功');
            router.goBack();
          } else {
            message.error(res.message);
          }
        });
      }
    });
  };

  render() {
    const { getFieldDecorator } = this.props.form;
    const { suppliers, products, locations } = this.state;

    const columns = [
      {
        title: (
          <span>
            <sup style={{ color: 'red' }}>*</sup>产品名称
          </span>
        ),
        key: 'productName',
        render: (text, row, index) => (
          <Select
            style={{ width: '80%' }}
            onChange={(value, option) =>
              this.saveProInfo({
                productId: value,
                productName: option.props.children,
              })
            }
          >
            {products.map(item => (
              <Option value={item.productId} key={item.productId}>
                {item.productName}
              </Option>
            ))}
          </Select>
        ),
        width: '25%',
      },
      {
        title: '质量要求',
        key: 'qualityRequirement',
        render: (text, row, index) => (
          <Input
            style={{ width: '80%' }}
            onChange={e => this.saveProInfo({ qualityRequirement: e.target.value })}
          />
        ),
        width: '25%',
      },
      {
        title: '合同数量',
        key: 'quantity',
        render: (text, row, index) => (
          <InputNumber
            style={{ width: '80%' }}
            onChange={value => this.saveProInfo({ quantity: value })}
          />
        ),
        width: '25%',
      },
      {
        title: '采购单价',
        key: 'unitPrice',
        render: (text, row, index) => (
          <InputNumber
            style={{ width: '80%' }}
            onChange={value => this.saveProInfo({ unitPrice: value })}
          />
        ),
        width: '25%',
      },
    ];

    return (
      <div className={styles.container}>
        <div className={styles.header}>
          <span className={styles.title}>新增采购合同</span>
          <div className={styles.btnBox}>
            <Button onClick={() => router.goBack()}>取消</Button>
            <Button onClick={this.handleAdd.bind(this)} type="primary">
              提交
            </Button>
          </div>
        </div>

        <Card title="合同概况">
          <Form>
            <FormItem label="合同编号">
              {getFieldDecorator('contractNum', {
                rules: [{ required: true, message: '请输入合同编号' }],
              })(<Input style={{ width: '75%' }} placeholder="请输入合同编号" />)}
            </FormItem>
            <FormItem label="供应商">
              {getFieldDecorator('supplierId', {
                rules: [{ required: true, message: '请选择供应商' }],
              })(
                <Select style={{ width: '75%' }} placeholder="请选择供应商">
                  {suppliers.map(item => (
                    <Option value={item.id} key={item.id}>
                      {item.name}
                    </Option>
                  ))}
                </Select>,
              )}
            </FormItem>
            <FormItem label="签订日期">
              {getFieldDecorator('signDate', {
                rules: [{ required: true, message: '请选择签订日期' }],
              })(
                <DatePicker
                  allowClear={false}
                  format={formatter}
                  style={{ width: '75%' }}
                  placeholder="请选择签订日期"
                />,
              )}
            </FormItem>
            <FormItem label="合同金额">
              {getFieldDecorator('amount')(
                <InputNumber
                  style={{ width: '75%', marginRight: 18 }}
                  placeholder="请输入合同金额"
                />,
              )}
              元
            </FormItem>
            <FormItem label="联系人">
              {getFieldDecorator('linkMan')(
                <Input style={{ width: '75%' }} placeholder="请输入联系人" />,
              )}
            </FormItem>
            <FormItem label="联系人手机号">
              {getFieldDecorator('linkPhone')(
                <Input style={{ width: '75%' }} placeholder="请输入手机号" />,
              )}
            </FormItem>
          </Form>
        </Card>

        <Card title="产品信息">
          <Table columns={columns} dataSource={[{ a: 1 }]} pagination={false} />
        </Card>

        <Card title="合同概况">
          <Form>
            <FormItem label="起始地">
              {getFieldDecorator('addressId', {
                rules: [{ required: true, message: '请选择起始地' }],
              })(
                <Select style={{ width: '75%' }} placeholder="请选择起始地">
                  {locations.map(item => (
                    <Option value={item.id} key={item.id}>
                      {item.name}
                    </Option>
                  ))}
                </Select>,
              )}
            </FormItem>
            <FormItem label="采购方式">
              {getFieldDecorator('purchaserMode', {
                rules: [{ required: true, message: '请选择采购方式' }],
              })(
                <Select style={{ width: '75%' }} placeholder="请选择采购方式">
                  <Option value={0}>自提</Option>
                  <Option value={1}>供方发货</Option>
                </Select>,
              )}
            </FormItem>
            <FormItem label="确认方">
              {getFieldDecorator('confirmPart', {
                rules: [{ required: true, message: '请选择确认方' }],
              })(
                <Select style={{ width: '75%' }} placeholder="请选择确认方">
                  <Option value={0}>我方确认</Option>
                  <Option value={1}>供应商确认</Option>
                </Select>,
              )}
            </FormItem>
            <FormItem label="合同文件" style={{ display: 'flex', alignItems: 'center' }}>
              {getFieldDecorator('files', {
                valuePropName: 'fileList',
                getValueFromEvent: this.handleOnChange,
              })(<NewUpload accept="" number={5} />)}
            </FormItem>
          </Form>
        </Card>

        <div className={styles.footer}>
          <div className={styles.btnBox}>
            <Button onClick={() => router.goBack()}>取消</Button>
            <Button onClick={this.handleAdd.bind(this)} type="primary">
              提交
            </Button>
          </div>
        </div>
      </div>
    );
  }
}
