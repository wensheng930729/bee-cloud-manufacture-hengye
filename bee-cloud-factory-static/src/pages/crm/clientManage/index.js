import { Component } from 'react';
import { Button, Card, Table, Form, Input, Row, Col, Select, Tooltip, message } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getList
} from './services/index';

const FormItem = Form.Item;
const Option = Select.Option;

const tabList = [
  {
    key: 'enable',
    tab: '启用客户'
  }, {
    key: 'disable',
    tab: '停用客户'
  }
]

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    key: 'enable',
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
    const { key } = this.state;
    validateFields((err, values) => {
      if (!err) {
        let params = {
          category: values.category !== undefined && values.category !== null ? values.category : undefined,
          name: values.name !== undefined && values.name !== null ? values.name : undefined,
          status: key === 'enable' ? 1 : 0
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
    this.setState({ key: 'enable' })
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  onTabChange = (key, type) => {
    this.setState({ [type]: key }, () => {
      this.getData({ currentPage: 1, pageSize: 10 });
    });
  };

  render() {
    const { getFieldDecorator, resetFields } = this.props.form;
    const { key, data, currentPage, pageSize, totalPage, totalRecords } = this.state;
    const columns = [
      {
        title: '客户名称',
        dataIndex: 'name',
        key: 'name',
        width: '20%'
      }, {
        title: '类别',
        dataIndex: 'category',
        key: 'category',
        render: (text, row) => text === 0 ? '核心客户' : text === 1 ? '战略客户' : text === 2 ? '一般客户' : '',
        width: '20%'
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) => text === 0 ? '禁用' : text === 1 ? '启用' : '',
        width: '20%'
      }, {
        title: '地址',
        dataIndex: 'address',
        key: 'address',
        render: (text, row) => text && text.length > 15 ? <Tooltip title={text}>{text.substring(0, 15)}...</Tooltip> : text,
        width: '20%'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => router.push(`/crm/clientManage/edit?id=${row.id}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>查询详情</span>
          </div>
        ),
        width: '20%'
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push(`/crm/clientManage/edit`)} >新增</Button>} />
        <div className={styles.container}>
          <Card
            tabList={tabList}
            bordered={false}
            activeTabKey={key}
            onTabChange={key => this.onTabChange(key, 'key')}
          >
            <Form layout='inline'>
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="客户名称">
                    {
                      getFieldDecorator('name')(
                        <Input style={{ width: 260 }} placeholder="按客户名称进行查询" />
                      )
                    }
                  </FormItem>
                  <FormItem label="客户类别">
                    {
                      getFieldDecorator('category')(
                        <Select style={{ width: 260 }}>
                          <Option value={0}>核心客户</Option>
                          <Option value={1}>战略客户</Option>
                          <Option value={2}>储备客户</Option>
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