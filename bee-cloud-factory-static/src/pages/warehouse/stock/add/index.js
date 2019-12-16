import { Component } from 'react';
import { Form, Button, Card, Table, Divider, Input, DatePicker, message, Select, InputNumber } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getOrderNumber,
  getProducts,
  getProductSpec,
  getWarehouse,
  saveStock
} from '../../services/index';
import moment from 'moment';

const FormItem = Form.Item;
const { TextArea } = Input;
const { Option } = Select;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    detailRQS: [],
    orderNumber: null,
    products: [],
    spec: [],
    warehouse: []
  }

  componentDidMount() {
    getOrderNumber().then(res => {
      if (res.code === 1) {
        this.setState({ orderNumber: res.object });
      } else {
        message.error(res.message);
      }
    })
    getProducts().then(res => {
      if (res.code === 1) {
        this.setState({ products: res.object });
      } else {
        message.error(res.message);
      }
    })
    getWarehouse().then(res => {
      if (res.code === 1) {
        this.setState({ warehouse: res.object });
      } else {
        message.error(res.message);
      }
    })
  }

  addRow = () => {
    const { detailRQS } = this.state;
    detailRQS.push({
      productId: null,
      productName: null,
      repositoryId: null,
      repositoryName: null,
      productSpecId: null,
      productSpecName: null,
      unit: null,
      testResult: null,
      quantity: null,
      price: null,
      money: null
    })
    this.setState({ detailRQS })
  }

  changeProduct = (index, productId, name) => {
    const { detailRQS } = this.state;
    getProductSpec(productId).then(res => {
      if (res.code === 1) {
        if (res.object) {
          this.setState({ spec: res.object });
        }
      } else {
        message.error(res.message);
      }
    })
    detailRQS[index] = {
      ...detailRQS[index],
      productId,
      productName: name.split('_')[0],
      unit: name.split('_')[1],
    }
    this.setState({ detailRQS })
  }

  changeSpec = (index, productSpecId, productSpecName) => {
    const { detailRQS } = this.state;
    detailRQS[index] = {
      ...detailRQS[index],
      productSpecId,
      productSpecName,
    }
    this.setState({ detailRQS })
  }

  changeWarehouse = (index, repositoryId, repositoryName) => {
    const { detailRQS } = this.state;
    detailRQS[index] = {
      ...detailRQS[index],
      repositoryId,
      repositoryName
    }
    this.setState({ detailRQS })
  }

  changeResult = (index, value) => {
    const { detailRQS } = this.state;
    detailRQS[index].testResult = value;
    this.setState({ detailRQS })
  }

  inputChange = (index, key, value) => {
    const { detailRQS } = this.state;
    detailRQS[index][key] = value;
    this.setState({ detailRQS })
  }

  save = () => {
    const { validateFields } = this.props.form;
    const { detailRQS } = this.state;

    validateFields((err, values) => {
      if (!err) {
        let params = {};
        console.log(values)

        params = {
          code: values.code,
          openingInventoryTime: moment(values.time).format("YYYY-MM-DD"),
          remark: values.remark ? values.remark : undefined,
          detailRQS
        }

        saveStock(params).then(res => {
          if (res.code === 1) {
            message.success(res.message);
            router.goBack();
          } else {
            message.error(res.message);
          }
        })
      }
    });
  }

  render() {
    const { getFieldDecorator, resetFields } = this.props.form;
    const { detailRQS, orderNumber, products, spec, warehouse } = this.state;
    const columns = [
      {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>产品</span>,
        key: 'productName',
        render: (text, row, index) => (
          <Select style={{ width: 148 }} onChange={(value, option) => this.changeProduct(index, value, option.props.children)}>
            {
              products.map((item, index) => <Option value={item.id} key={item.id}>{item.name + '_' + item.unitValue}</Option>)
            }
          </Select>
        ),
        width: '12.5%'
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>规格</span>,
        key: 'productSpecName',
        render: (text, row, index) => (
          <Select style={{ width: 148 }} onChange={(value, option) => this.changeSpec(index, value, option.props.children)}>
            {
              spec.map((item, index) => <Option value={item.id} key={item.id}>{item.specName}</Option>)
            }
          </Select>
        ),
        width: '12.5%'
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>仓库</span>,
        key: 'repositoryName',
        render: (text, row, index) => (
          <Select style={{ width: 148 }} onChange={(value, option) => this.changeWarehouse(index, value, option.props.children)}>
            {
              warehouse.map((item, index) => <Option value={item.id} key={item.id}>{item.name}</Option>)
            }
          </Select>
        ),
        width: '12.5%'
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>计量单位</span>,
        key: 'unit',
        dataIndex: 'unit',
        width: '12.5%'
      }, {
        title: '化验结果',
        key: 'testResult',
        render: (text, row, index) => (
          <Select style={{ width: 148 }} onChange={(value, option) => this.changeResult(index, value)}>
            <Option value="合格">合格</Option>
            <Option value="不合格">不合格</Option>
          </Select>
        ),
        width: '12.5%'
      }, {
        title: <span><sup style={{ color: '#f5222d' }}>*</sup>期初数量</span>,
        key: 'quantity',
        render: (text, row, index) => (<InputNumber style={{ width: 148 }} onChange={(value) => this.inputChange(index, 'quantity', value)} />),
        width: '12.5%'
      }, {
        title: '期初单价',
        key: 'price',
        render: (text, row, index) => (<InputNumber style={{ width: 148 }} onChange={(value) => this.inputChange(index, 'price', value)} />),
        width: '12.5%'
      }, {
        title: '期初金额',
        key: 'money',
        render: (text, row, index) => (<InputNumber style={{ width: 148 }} onChange={(value) => this.inputChange(index, 'money', value)} />),
        width: '12.5%'
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回列表</Button>} />
        <div className={styles.container}>
          <Card title="新增期初库存" bordered={false}>
            <Form>
              <FormItem label="单号">
                {
                  getFieldDecorator('code', {
                    initialValue: orderNumber,
                    rules: [{ required: true, message: '请输入' }]
                  })(
                    <Input style={{ width: 320 }} placeholder="模糊查询" />
                  )
                }
              </FormItem>
              <FormItem label="期初日期">
                {
                  getFieldDecorator('times')(
                    <DatePicker allowClear={false} style={{ width: 320 }} format="YYYY-MM-DD" />
                  )
                }
              </FormItem>
              <FormItem label="备注">
                {
                  getFieldDecorator('remark')(
                    <TextArea style={{ width: 320 }} autosize={{ minRows: 4, maxRows: 4 }} />
                  )
                }
              </FormItem>
            </Form>
            <Divider />
            <p className={styles.tableTitle}>期初明细</p>
            <Table
              rowKey={(row, index) => index}
              columns={columns}
              dataSource={detailRQS}
              pagination={false}
            />
            <Button onClick={this.addRow} type="dashed" block style={{ marginTop: 16 }}>添加一行</Button>

            <div className={styles.btnBox}>
              <Button onClick={() => router.goBack()}>取消</Button>
              <Button onClick={this.save} type="primary" style={{ marginLeft: 16 }}>保存</Button>
            </div>
          </Card>
        </div >
      </div >
    )
  }
}