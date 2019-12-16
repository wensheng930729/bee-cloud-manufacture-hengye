import { Component } from 'react';
import { Button, Card, Table, Form, Input, Select, Modal, message, Row, Col } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../components/BreadCrumb';
import router from 'umi/router';
import { getList, enable, disable } from './services/index';

const FormItem = Form.Item;
const Option = Select.Option;

//仓库档案文件
@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0,
    visible: false,
    editObj: {
      code: null,
      name: null,
      status: 0
    }
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData = ({ currentPage, pageSize }) => {
    const { getFieldValue } = this.props.form;
    let params = {
      currentPage,
      pageSize,
      name: getFieldValue("temp_name") || undefined
    };
    getList(params).then(res => {
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

  onChange = (currentPage) => {
    const { pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  onShowSizeChange = (currentPage, pageSize) => {
    this.getData({ currentPage, pageSize });
  }

  handleUpdate = () => {
    const { validateFields, resetFields } = this.props.form;
    const { editObj } = this.state;
    let self = this;
    validateFields(['status'], (err, values) => {
      if (!err) {
        let params = {
          code: editObj.code,
          id: editObj.id,
          name: editObj.name
        };

        if (values.status) {
          enable(params).then(res => {
            if (res && res.code === 1) {
              message.success(res.message);
              self.setState({ visible: false, editObj: { code: null, name: null, status: 0 } });
              self.getData({ currentPage: 1, pageSize: 10 });
              resetFields();
            } else {
              message.error(res.message);
            }
          })
        } else {
          disable(params).then(res => {
            if (res && res.code === 1) {
              message.success(res.message);
              self.setState({ visible: false, editObj: { code: null, name: null, status: 0 } });
              self.getData({ currentPage: 1, pageSize: 10 });
              resetFields();
            } else {
              message.error(res.message);
            }
          })
        }
      }
    });
  }

  handleReset = () => {
    const { resetFields } = this.props.form;
    resetFields();
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  handleSearch = () => {
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { data, currentPage, pageSize, totalPage, totalRecords, visible, editObj } = this.state;
    const columns = [
      {
        title: '看板id',
        dataIndex: 'code',
        key: 'code',
        width: '25%'
      }, {
        title: '看板名称',
        dataIndex: 'name',
        key: 'name',
        width: '25%'
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) => text ? "启用" : "禁用",
        width: '25%'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => this.setState({ visible: true, editObj: row })} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
          </div>
        ),
        width: '25%'
      }
    ]

    return (
      <div>
        <BreadCrumb />

        <div className={styles.container}>
          <Card title="看板BI配置" bordered={false}>
            <Form layout='inline'>
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="看板名称">
                    {
                      getFieldDecorator('temp_name')(
                        <Input style={{ width: 260 }} />
                      )
                    }
                  </FormItem>
                </Col>
                <Col>
                  <Button onClick={this.handleReset} style={{ marginRight: 24 }}>重置</Button>
                  <Button type="primary" onClick={this.handleSearch}>查询</Button>
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

        <Modal
          title="编辑看板BI配置"
          visible={visible}
          onOk={this.handleUpdate}
          okText="保存"
          onCancel={() => { this.setState({ visible: false, editObj: { code: null, name: null, status: 0 } }) }}
        >
          <Form layout="inline">
            <FormItem label="看板id">
              {
                getFieldDecorator('code', {
                  initialValue: editObj.code,
                  rules: [{ required: true, message: '请输入仓库名称' }]
                })(
                  <Input style={{ width: 300 }} disabled />
                )
              }
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="看板名称">
              {
                getFieldDecorator('name', {
                  initialValue: editObj.name,
                  rules: [{ required: true, message: '请输入仓库名称' }]
                })(
                  <Input style={{ width: 300 }} disabled />
                )
              }
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="状态">
              {
                getFieldDecorator('status', {
                  initialValue: editObj.status,
                  rules: [{ required: true, message: '请选择状态' }]
                })(
                  <Select style={{ width: 300 }} placeholder="请选择">
                    <Option value={0}>禁用</Option>
                    <Option value={1}>启用</Option>
                  </Select>
                )
              }
            </FormItem>
          </Form>
        </Modal>
      </div>
    )
  }
}