import { Component } from 'react';
import { Form, Input, Select, DatePicker, Button, Table } from 'antd';
import styles from './index.less';
import withRouter from 'umi/withRouter';
import router from 'umi/router';
import * as utils from '@/utils/utils';
import { getCustomerListByCategory, getWeightMachineWebList } from '../../services/index';
import moment from 'moment';

const FormItem = Form.Item;
const { RangePicker } = DatePicker;
const Option = Select.Option;
const formatter = 'YYYY-MM-DD HH:mm:ss';

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    customer: [],
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0,
  };

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    getCustomerListByCategory().then(res => {
      if (res && res.code === 1) {
        this.setState({ customer: res.object });
      }
    });
    this.getData({ currentPage, pageSize });
  }

  getData = pages => {
    const { validateFields } = this.props.form;
    validateFields((err, values) => {
      if (!err) {
        let params = {
          ...values,
          times: undefined,
          startTime:
            values.times && values.times.length && values.times.length !== 0
              ? moment(values.times[0]).format(formatter)
              : undefined,
          endTime:
            values.times && values.times.length && values.times.length !== 0
              ? moment(values.times[1]).format(formatter)
              : undefined,
          isWeight: 0,
          type: 2,
        };
        const str = utils.queryString(pages);
        getWeightMachineWebList(str, params).then(res => {
          if (res && res.code === 1 && res.object) {
            this.setState({
              data: res.object,
              ...res.page,
            });
          }
        });
      }
    });
  };

  onChange = currentPage => {
    const { pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  };

  onShowSizeChange = (currentPage, pageSize) => {
    this.getData({ currentPage: 1, pageSize });
  };

  handleSearch = () => {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage: 1, pageSize });
  };

  handleReset = () => {
    const { resetFields } = this.props.form;
    resetFields();
    this.getData({ currentPage: 1, pageSize: 10 });
  };

  render() {
    const { getFieldDecorator } = this.props.form;
    const { customer, data, currentPage, pageSize, totalPage, totalRecords } = this.state;

    const columns = [
      {
        title: '磅单号',
        dataIndex: 'machineId',
        key: 'machineId',
        width: '15%',
      },
      {
        title: '过磅车辆号',
        dataIndex: 'trainNumber',
        key: 'trainNumber',
        width: '15%',
      },
      {
        title: '收货单位',
        dataIndex: 'receivingCompany',
        key: 'receivingCompany',
        width: '15%',
      },
      {
        title: '承运商',
        dataIndex: 'carrierName',
        key: 'carrierName',
        width: '15%',
      },
      {
        title: '司机',
        dataIndex: 'driver',
        key: 'driver',
        width: '15%',
      },
      {
        title: '创建日期',
        dataIndex: 'createTime',
        key: 'createTime',
        width: '15%',
      },
      {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <span
            onClick={() => router.push(`/weighbridge/sale/edit?machineId=${row.machineId}`)}
            style={{ color: '#1890ff', cursor: 'pointer' }}
          >
            开始称重
          </span>
        ),
        width: '10%',
      },
    ];

    return (
      <div className={styles.container}>
        <div>
          <Form layout="inline">
            <FormItem label="车辆号">
              {getFieldDecorator('trainNumber')(
                <Input style={{ width: 240 }} placeholder="按过磅车辆号查询" />,
              )}
            </FormItem>
            <FormItem label="合同号">
              {getFieldDecorator('contractNum')(
                <Input style={{ width: 240 }} placeholder="按合同号查询" />,
              )}
            </FormItem>
            <FormItem label="创建日期">
              {getFieldDecorator('times')(
                <RangePicker allowClear={false} style={{ width: 280 }} format={formatter} />,
              )}
            </FormItem>
            <FormItem label="收货单位">
              {getFieldDecorator('supOrCustId')(
                <Select style={{ width: 220 }}>
                  {customer.map(item => (
                    <Option value={item.id} key={item.id}>
                      {item.name}
                    </Option>
                  ))}
                </Select>,
              )}
            </FormItem>
          </Form>
          <div className={styles.btnBox}>
            <Button onClick={() => router.push('/weighbridge/sale/add')} type="primary" icon="plus">
              新增称重
            </Button>
            <div>
              <Button onClick={this.handleReset} style={{ marginRight: 24 }}>
                重置
              </Button>
              <Button onClick={this.handleSearch} type="primary">
                查询
              </Button>
            </div>
          </div>
        </div>

        <Table
          columns={columns}
          dataSource={data}
          rowKey="machineId"
          pagination={{
            showQuickJumper: true,
            showSizeChanger: true,
            defaultCurrent: 1,
            defaultPageSize: 10,
            current: currentPage,
            pageSize: pageSize,
            total: totalRecords,
            onChange: this.onChange.bind(this),
            pageSizeOptions: ['10', '20', '30'],
            showTotal: (total, range) =>
              `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
            onShowSizeChange: this.onShowSizeChange.bind(this),
          }}
        />
      </div>
    );
  }
}
