import { Component } from 'react';
import { Button, Card, Table, Divider, Form, Input, DatePicker, Row, Col } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../components/BreadCrumb';
import router from 'umi/router';
import moment from 'moment';
import {
  getStocks
} from '../services/index';

const FormItem = Form.Item;
const { MonthPicker, RangePicker, WeekPicker } = DatePicker;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData = ({ currentPage, pageSize }) => {
    const { validateFields } = this.props.form;
    validateFields((err, values) => {
      if (!err) {
        let params = {
          code: values.code !== undefined && values.code !== null ? values.code : undefined,
          startTime: values.times && values.times.length !== 0 ? moment(values.times[0]).format("YYYY-MM-DD") : undefined,
          endTime: values.times && values.times.length !== 0 ? moment(values.times[1]).format("YYYY-MM-DD") : undefined,
        };

        getStocks({ currentPage, pageSize, params }).then(res => {
          if (res && res.code === 1 && res.object) {
            this.setState({
              data: res.object,
              ...res.page
            })
          } else {
            message.error(res.message);
          }
        })
      }
    });
  }

  onChange = (currentPage) => {
    const { pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  onShowSizeChange = (currentPage, pageSize) => {
    this.getData({ currentPage, pageSize });
  }

  handleSearch = () => {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  handleReset = () => {
    const { resetFields } = this.props.form;
    resetFields();
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  render() {
    const { getFieldDecorator, resetFields } = this.props.form;
    const { data, currentPage, pageSize, totalPage, totalRecords } = this.state;
    const columns = [
      {
        title: '单号',
        dataIndex: 'code',
        key: 'code',
        width: '25%'
      }, {
        title: '期初日期',
        dataIndex: 'openingInventoryTime',
        key: 'openingInventoryTime',
        width: '25%'
      }, {
        title: '备注',
        dataIndex: 'remark',
        key: 'remark',
        width: '25%'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => router.push(`/warehouse/stock/detail?id=${row.id}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>查询详情</span>
          </div>
        ),
        width: '25%'
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push(`/warehouse/stock/add`)} >新增</Button>} />
        <div className={styles.container}>
          <Card title="期初库存" bordered={false}>
            <Form layout='inline'>
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="单号">
                    {
                      getFieldDecorator('code')(
                        <Input style={{ width: 260 }} placeholder="模糊查询" />
                      )
                    }
                  </FormItem>
                  <FormItem label="期初日期">
                    {
                      getFieldDecorator('times')(
                        <RangePicker style={{ width: 320 }} format="YYYY-MM-DD" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col>
                  <Button type="primary" onClick={this.handleSearch}>查询</Button>
                  <Button onClick={this.handleReset} style={{ marginLeft: 24 }}>重置</Button>
                </Col>
              </Row>
            </Form>
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data}
              pagination={{
                showQuickJumper: true,
                showSizeChanger: true,
                defaultCurrent: 1,
                defaultPageSize: 10,
                current: currentPage,
                pageSize: pageSize,
                total: totalRecords,
                onChange: this.onChange.bind(this),
                pageSizeOptions: ["10", "20", "30"],
                showTotal: (total, range) => `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
                onShowSizeChange: this.onShowSizeChange.bind(this)
              }}
            />
          </Card>
        </div>
      </div>
    )
  }
}