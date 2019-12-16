import { Component } from 'react';
import { Button, Card, Table, Form, Input, Row, Col, Select, message } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getList
} from './services/index';

const FormItem = Form.Item;
const Option = Select.Option;

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
          name: values.name !== undefined && values.name !== null ? values.name : undefined,
          type: values.type !== undefined && values.type !== null ? values.type : undefined
        };

        getList({ currentPage, pageSize, params }).then(res => {
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
    const { getFieldDecorator } = this.props.form;
    const { data, currentPage, pageSize, totalPage, totalRecords } = this.state;
    const columns = [
      {
        title: '公司名称',
        dataIndex: 'name',
        key: 'name',
        width: '17%'
      }, {
        title: '类别',
        dataIndex: 'type',
        key: 'type',
        render: (text, row) => text === 0 ? '客户' : text === 1 ? '供应商' : '',
        width: '17%'
      }, {
        title: '人数',
        dataIndex: 'num',
        key: 'num',
        width: '16%'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => router.push(`/crm/accountManage/list?id=${row.id}&type=${row.type}&name=${row.name}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>查询详情</span>
          </div>
        ),
        width: '17%'
      }
    ]
    return (
      <div>
        <BreadCrumb />
        <div className={styles.container}>
          <Card title="上下游账户管理" bordered={false} >
            <Form layout='inline'>
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="公司名称">
                    {
                      getFieldDecorator('name')(
                        <Input style={{ width: 260 }} placeholder="按公司名称进行查询" />
                      )
                    }
                  </FormItem>
                  <FormItem label="公司类别">
                    {
                      getFieldDecorator('category')(
                        <Select style={{ width: 260 }}>
                          <Option value={0}>客户</Option>
                          <Option value={1}>供应商</Option>
                        </Select>
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