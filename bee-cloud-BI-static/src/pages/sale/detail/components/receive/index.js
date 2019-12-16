import React, { PureComponent } from 'react';
import { Card, Table, Dropdown, Menu, Icon, Select, Form, InputNumber, Input, message } from 'antd';
import { connect } from 'dva';
import withRouter from 'umi/withRouter';
import styles from './index.less';
import constants from '../../../constants';
import { MadalHOC } from '@/components';
import {
  saveDiscountTransportDetail,
  saveAssayResult,
  getCarSampleInfo,
} from '../../../services/services';
import { getTestAttributeOutByProductId } from '@/services/index';

@withRouter
@connect(({ sale: { tableDatas, orderInfo } }) => ({ tableDatas, orderInfo }))
export default class Index extends PureComponent {
  state = {
    sampleInfo: [], // 化验结果数据
    showReslut: false, // 化验结果
    discountInfo: {}, // 折价入库数据
    showDiscount: false, // 折价入库
    resultAttributes: [], // 化验项 输入列表
    showInStorage: false, // 确定入库
    inStorageInfo: {},
  };

  componentDidMount() {
    this.contractBusinessId = this.props.location.query.contractBusinessId;
    this.getData({});
  }

  // 获取列表数据
  getData({ currentPage = 1 }) {
    const { dispatch } = this.props;
    dispatch({
      type: 'sale/getCarList',
      payload: { contractBusinessId: this.contractBusinessId, currentPage, pageSize: 5 },
    });
  }

  // 翻页
  onChange({ current }) {
    this.getData({ currentPage: current });
  }

  // 查看化验结果
  showResult(carrierTransportDetailId) {
    getCarSampleInfo({ carrierTransportDetailId }).then(res => {
      if (res.code === 1) {
        this.setState({
          sampleInfo: res.object,
          showReslut: true,
        });
      }
    });
  }

  // 更多操作
  moreOptionClick(row, { key }) {
    const {
      orderInfo: { contract = {} },
    } = this.props;
    switch (key) {
      case '1':
        // 获取化验结果项 表单列表
        getTestAttributeOutByProductId(contract.productId).then(res => {
          if (res.code === 1) {
            this.setState({
              inStorageInfo: row,
              resultAttributes: res.object,
              showInStorage: true,
            });
          } else {
            message.error(res.message);
          }
        });
        break;
      case '2':
        this.setState({
          discountInfo: row,
          showDiscount: true,
        });
        break;
      default:
        break;
    }
  }

  // 合同确认收货入库
  saveAssayResult = values => {
    const { inStorageInfo, resultAttributes } = this.state;
    let params = {
      assayResult: values.assayResult,
      carrierTransportDetailIds: [inStorageInfo.carrierTransportDetailId],
      resultList: resultAttributes.map(item => ({
        ...item,
        assayItem: item.assayItemOut,
        assayValue: values[item.assayItemOut],
      })),
    };

    saveAssayResult(params).then(res => {
      if (res.code === 1) {
        message.success(res.message);
        this.getData({});
        this.setState({ showInStorage: false });
      } else {
        message.error(res.message);
      }
    });
  };

  // 被折价
  saveDiscountTransportDetail = (info, values) => {
    const params = { ...values, carrierTransportDetailIds: [info.carrierTransportDetailId] };
    saveDiscountTransportDetail(params).then(res => {
      if (res.code === 1) {
        this.getData({});
        this.setState({
          showDiscount: false,
        });
      }
      message.info(res.message);
    });
  };

  render() {
    const {
      showReslut,
      sampleInfo,
      showDiscount,
      discountInfo,
      resultAttributes,
      showInStorage,
    } = this.state;
    const {
      tableDatas,
      orderInfo: { contract = {} },
    } = this.props;
    const { data = [], page = {} } = tableDatas[constants.SALE_RECEIVE.key];
    const pagination = {
      current: page.currentPage || 1,
      total: page.totalRecords || 0,
      pageSize: page.pageSize || 6,
    };

    // 化验结果弹出层属性
    const ResultMadalProps = {
      visible: showReslut,
      title: '查看化验结果',
      onOk: () => this.setState({ showReslut: false }),
      onCancel: () => this.setState({ showReslut: false }),
      okText: '确定',
      cancelText: '取消',
      ComProps: { data: sampleInfo },
    };

    // 折价入库弹出层属性
    const DiscountMadalProps = {
      visible: showDiscount,
      title: '被折价',
      onOk: this.saveDiscountTransportDetail.bind(this, discountInfo),
      onCancel: () => this.setState({ showDiscount: false }),
      okText: '确定',
      cancelText: '取消',
      ComProps: { data: { ...discountInfo, ...contract } },
    };

    // 折价入库弹出层属性
    const InStorageMadalProps = {
      visible: showInStorage,
      title: '确认收货:请填写客户化验结果',
      onOk: this.saveAssayResult.bind(this),
      onCancel: () => this.setState({ showInStorage: false }),
      okText: '确定',
      cancelText: '取消',
      ComProps: { data: resultAttributes },
    };
    // 表头
    const columns = [...constants.SALE_RECEIVE.columns];
    columns.push({
      title: '操作',
      align: 'center',
      dataIndex: '_option',
      key: '_option',
      render: (text, row) => (
        <>
          <a onClick={this.showResult.bind(this, row.carrierTransportDetailId)}>查看化验结果</a>
          　&nbsp;
          {row.handleType === undefined ? (
            <Dropdown overlay={menu(row)}>
              <a className="ant-dropdown-link">
                更多操作 <Icon type="down" />
              </a>
            </Dropdown>
          ) : null}{' '}
        </>
      ),
    });

    // 更多操作
    const menu = row => (
      <Menu onClick={this.moreOptionClick.bind(this, row)}>
        <Menu.Item key={1}>
          <a target="_blank">确认收货</a>
        </Menu.Item>
        <Menu.Item key={2}>
          <a target="_blank">被折价</a>
        </Menu.Item>
        {/* <Menu.Item key={3}>
          <a target="_blank">
            退货
          </a>
        </Menu.Item> */}
      </Menu>
    );
    return (
      <>
        <CardTable
          columns={columns}
          data={data}
          pagination={pagination}
          name={constants.SALE_RECEIVE.name}
          onChange={this.onChange.bind(this)}
        />
        <ResultWraper {...ResultMadalProps} />
        <DiscountWraper {...DiscountMadalProps} />

        <InStorageWraper {...InStorageMadalProps} />
      </>
    );
  }
}

// 产品信息
const CardTable = ({ name, columns, data, pagination, onChange }) => (
    <Card title={name} className={styles.cardTable}>
      <Table columns={columns} dataSource={data} pagination={pagination} onChange={onChange} />
    </Card>
  );

// 化验结果
const Result = ({ data = {} }) => (
    <div className={styles.result}>
      <>
        <section>
          <span>
            化验结果：
            <ul>
              {data && data.length ? (
                data.map(_item => (
                  <>
                    <li>
                      <div>{_item.assayItem}</div>
                      <div>{_item.assayValue + '' + _item.unitString}</div>
                    </li>
                  </>
                ))
              ) : (
                <li>无</li>
              )}{' '}
            </ul>{' '}
          </span>
        </section>
      </>
    </div>
  );

const ResultWraper = MadalHOC(Result);

// 被折价Modal
class Discount extends PureComponent {
  render() {
    const {
      data,
      form: { getFieldDecorator },
    } = this.props;
    const formItemLayout = {
      labelCol: {
        xs: { span: 12 },
        sm: { span: 6 },
      },
      wrapperCol: {
        xs: { span: 24 },
        sm: { span: 14 },
      },
    };
    const config = {
      rules: [{ required: true }],
    };
    return (
      <Form {...formItemLayout} title="请填写客户化验结果">
        <Form.Item label="客户">{data.customerName}</Form.Item>
        <Form.Item label="产品名称">{data.productName}</Form.Item>
        <Form.Item label="载重数量">{data.cargoWeight || 0}吨</Form.Item>
        <Form.Item label="折后单价">
          {getFieldDecorator('discountUnitPrice', config)(<InputNumber />)}
        </Form.Item>
      </Form>
    );
  }
}

const DiscountWraper = MadalHOC(Form.create()(Discount));

class InStorage extends PureComponent {
  render() {
    const {
      data = [],
      form: { getFieldDecorator },
    } = this.props;
    const formItemLayout = {
      labelCol: {
        xs: { span: 24 },
        sm: { span: 8 },
      },
      wrapperCol: {
        xs: { span: 24 },
        sm: { span: 16 },
      },
    };

    return (
      <Form {...formItemLayout}>
        {data &&
          data.map(item => (
            <Form.Item label={item.assayItemOut}>
              {getFieldDecorator(item.assayItemOut, {
                rules: [{ required: true, message: '请输入' }],
              })(<Input suffix={item.unitString} />)}
            </Form.Item>
          ))}
        <Form.Item label="是否合格">
          {getFieldDecorator('assayResult', {
            rules: [{ required: true, message: '请选择是否合格' }],
          })(
            <Select placeholder="选择是否合格">
              <Option value={1}>合格</Option>
              <Option value={0}>不合格</Option>
            </Select>,
          )}
        </Form.Item>
      </Form>
    );
  }
}

const InStorageWraper = MadalHOC(Form.create()(InStorage));
